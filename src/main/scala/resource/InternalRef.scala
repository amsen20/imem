package imem.resource

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
class InternalRef[T](val unsafeRef: UnsafeRef[T]):

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
    */
  @throws(classOf[IllegalStateException])
  def newMut(derived: InternalRef[T]#Tag): MutRef[T] =
    useCheck(derived)
    currentTimeStamp += 1
    val newTag = Tag.Uniq(currentTimeStamp)
    stack.borrows.push(newTag)

    /** TODO: consider instead of passing a tag and this class to the reference, pass an
      * implementation of an interface that allows the reference to do the borrowing and accessing.
      */
    MutRef(newTag, this)

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
  def newSharedRef(derived: InternalRef[T]#Tag): ImmutRef[T] =
    readCheck(derived)
    currentTimeStamp += 1
    val newTag = Tag.Shr(currentTimeStamp)
    stack.borrows.push(newTag)
    ImmutRef(newTag, this)

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
          case Tag.Shr(ts)  => true
        )
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

  /** TODO: Should be private, it is used for implementing moving
    */
  def unsafeGet(): T = unsafeRef.unsafeGet()

  def drop(): Unit =
    stack.borrows.popAll()

end InternalRef

object InternalRef:
  def newWithTag[T](value: T): (InternalRef[T], InternalRef[T]#Tag) =
    val ref = new InternalRef(UnsafeRef(value))
    // TODO: make it cleaner, looks like a dirty way to create a new tag.
    val firstTag = ref.Tag.Uniq(ref.currentTimeStamp)
    ref.currentTimeStamp += 1
    ref.stack.borrows.push(firstTag)
    (ref, firstTag)
end InternalRef
