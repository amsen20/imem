package imem

import language.experimental.captureChecking

/** This class implemented the stacked borrows memory model mentioned in the paper in this link:
  * https://plv.mpi-sws.org/rustbelt/stacked-borrows/paper.pdf
  *
  * The implementation is simplified, to be exact:
  *   - It supports only `Unique`, and `SharedRO` pointers (i.e does not support shared read write
  *     raw pointers).
  *   - It assumes all pointers are safe and outside `UnsafeCell`.
  *   - It does not support re-tagging, meaning it assumes all tags are unique.
  *   - It does not support protectors, meaning it's possible that a reference (that is a function
  *     argument) does not out-live the function.
  *
  * This implementation is not high-performant, and will harm the concurrency, if shared between
  * multiple threads.
  *
  * The main reason that protection and re-tagging are not supported is that they require language
  * support (they should be added in the AST). But this class (at least for now) does not have
  * access to all the program that is using its instances.
  */
class InternalRef[T](var unsafeRef: UnsafeRef[T]):

  type Timestamp = Long

  enum Tag(val timestamp: Timestamp):
    // Uniq tag grants reads and writes.
    case Uniq(override val timestamp: Timestamp) extends Tag(timestamp)
    // Shr tag grants read-only access.
    case Shr(override val timestamp: Timestamp) extends Tag(timestamp)
  end Tag

  class Stack:
    // TODO: change it back to `Tag`, and don't expose tags out of `InternalRef`
    val borrows = scala.collection.mutable.Stack[InternalRef[T]#Tag]()
  end Stack

  private var currentTimeStamp = 0
  private val stack = Stack()

  /** Rule (NEW-MUTABLE-REF)
    *
    * TODO: first consider returning errors, instead of throwing exceptions. If throwing is the
    * decision, check if `@throws` is the correct way to inform the API user about it, or it should
    * be part of the return type annotation (e.g. `throws[Unit]`). Also, check if
    * `IllegalStateException` is the correct exception to throw in this case. Should a package
    * specific defined exception being thrown?
    *
    * TODO: Checkout `import language.experimental.saferExceptions`.
    */
  @throws(classOf[IllegalStateException])
  def newMut(derived: InternalRef[T]#Tag): InternalRef[T]#Tag =
    useCheck(derived)
    currentTimeStamp += 1
    val newTag = Tag.Uniq(currentTimeStamp)
    stack.borrows.push(newTag)

    /** TODO: consider instead of passing a tag and this class to the reference, pass an
      * implementation of an interface that allows the reference to do the borrowing and accessing.
      */
    newTag

  /** Rule (USE-1)
    *
    * TODO: the same as `newMut`
    */
  @throws(classOf[IllegalStateException])
  def useCheck(tag: InternalRef[T]#Tag): Unit =
    tag match
      case Tag.Uniq(ts) =>
        stack.borrows.popWhile(_ != tag)
        if stack.borrows.isEmpty then
          throw new IllegalStateException(s"UB: use after free detected for tag $tag")
      case Tag.Shr(ts) =>
        throw new IllegalStateException(s"Cannot use (modify) a pointer with `Shr` tag: $tag")

  /** Rule (NEW-SHARED-REF-1)
    *
    * TODO: the same as `newMut`
    */
  @throws(classOf[IllegalStateException])
  def newSharedRef(derived: InternalRef[T]#Tag): InternalRef[T]#Tag =
    readCheck(derived)
    currentTimeStamp += 1
    val newTag = Tag.Shr(currentTimeStamp)
    stack.borrows.push(newTag)
    newTag

  /** Rule (READ-1)
    *
    * TODO: the same as `newMut`
    */
  @throws(classOf[IllegalStateException])
  def readCheck(tag: InternalRef[T]#Tag): Unit =
    val above = stack.borrows.popWhile(item => item.timestamp != tag.timestamp)
    if stack.borrows.isEmpty then
      throw new IllegalStateException(s"UB: use after free detected for tag $tag")
    else
      above.reverse
        .takeWhile(_ match
          case Tag.Uniq(ts) => false
          case Tag.Shr(ts)  => true)
        .foreach(stack.borrows.push(_))

  /** TODO: the same as `newMut`
    */
  @throws(classOf[IllegalStateException])
  def read[S](tag: InternalRef[T]#Tag, readAction: T => S): S =
    readCheck(tag)
    unsafeRef.read(readAction)

  /** TODO: the same as `newMut`
    */
  @throws(classOf[IllegalStateException])
  def write[S](tag: InternalRef[T]#Tag, writeAction: T => S): S =
    useCheck(tag)
    unsafeRef.modify(writeAction)

  def writeWithLinearArg[S, LinearArgType <: scinear.Linear](
      tag: InternalRef[T]#Tag,
      linearArg: LinearArgType^,
      writeAction: (T, LinearArgType^{linearArg}) => S,
  ): S =
    useCheck(tag)
    unsafeRef.modifyWithLinearArg[S, LinearArgType](linearArg, writeAction)

  /** TODO: Should be private, it is used for implementing moving
    */
  def unsafeGet(): T = unsafeRef.unsafeGet()

  def setValue(value: T): Unit =
    // TODO: Here, `unsafeRef` can be freed.
    unsafeRef = UnsafeRef(value)

  def dropAllBorrows(): Unit =
    val removedTags = stack.borrows.popAll()
    stack.borrows.push(removedTags.last)

  def drop(): Unit =
    stack.borrows.popAll()

  override def toString(): String = s"InternalRef(${unsafeRef})"

end InternalRef

object InternalRef:
  def newWithTag[T](value: T): (InternalRef[T], InternalRef[T]#Tag) =
    val ref = new InternalRef(UnsafeRef(value))
    // TODO: make it cleaner, looks like a dirty way to create a new tag.
    val firstTag = ref.Tag.Uniq(ref.currentTimeStamp)
    ref.stack.borrows.push(firstTag)
    (ref, firstTag)

  def swap[T](
      tag1: InternalRef[T]#Tag,
      ref1: InternalRef[T],
      tag2: InternalRef[T]#Tag,
      ref2: InternalRef[T]
  ): Unit =
    /** Act like you are writing to them.
      */
    ref1.write(tag1, _ => ())
    ref2.write(tag2, _ => ())

    var tmp = ref1.unsafeRef
    ref1.unsafeRef = ref2.unsafeRef
    ref2.unsafeRef = tmp
end InternalRef
