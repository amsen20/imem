package imem

import language.experimental.captureChecking

/** TODO: Maybe make it an enum, and implement the internals in the Box methods.
  */
trait BoxImpl[T, +Owner^]:
  def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner]
  def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner]

  def name: String
  override def toString(): String

case class Uninitialized[T, +Owner^]() extends BoxImpl[T, Owner]:
  override def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def name: String = "Uninitialized"
  override def toString(): String = "Uninitialized"
end Uninitialized

case class Live[T, +Owner^](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]) extends BoxImpl[T, Owner]:
  override def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)
  override def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner] =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParents)
  override def name: String = "Live"
  override def toString(): String = s"Live(tag: ${tag}, internalRef: ${internalRef})"
end Live

case class Dropped[T, +Owner^]() extends BoxImpl[T, Owner]:
  override def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner] = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner] = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def name: String = "Dropped"
  override def toString(): String = "Dropped"
end Dropped

/**
 * TODO: For now, `Owner` can be any capability, and it may open ways to exploits.
 * Should restrict it to a specific type only.
*/
case class Box[T, +Owner^]():
  /** Ensure `Box`captures the owner without storing it in a field.
  */
  self: Box[T, Owner]^{Owner} =>

  // TODO: Looks dirty for accessing the inner type. Should find a better way.
  var Impl: BoxImpl[T, {Owner, this}] = Uninitialized()

  @throws(classOf[IllegalStateException])
  def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner, this}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner] = Impl.borrowImmut

  @throws(classOf[IllegalStateException])
  def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner, this}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner] = Impl.borrowMut

  @throws(classOf[IllegalStateException])
  def set(value: T): Unit =
    Impl match
      // ?: why should I mentioned [T, Owner] every time?
      // ?: Also, why is `asInstanceOf[BoxImpl[T, Owner]]` needed?
      case Uninitialized[T, {Owner, this}]() =>
        val (ref, tag) = InternalRef.newWithTag(value)
        Impl = Live[T, {Owner, this}](tag, ref).asInstanceOf[BoxImpl[T, {this, Owner}]]
      case Live[T, {Owner, this}](_, _) =>
        drop()
        val (ref, tag) = InternalRef.newWithTag(value)
        Impl = Live(tag, ref).asInstanceOf[BoxImpl[T, {Owner, this}]]
      case Dropped[T, {Owner, this}]() =>
        throw new IllegalStateException("Cannot set a value to a dropped Box")

  @throws(classOf[IllegalStateException])
  def drop(): Unit =
    Impl match
      case Uninitialized[T, {Owner, this}]() =>
        throw new IllegalStateException("Cannot drop an uninitialized Box")
      case Live[T, {Owner, this}](_, ref) =>
        ref.drop()
        Impl = Dropped()
      case Dropped[T, {Owner, this}]() =>
        throw new IllegalStateException("Cannot drop an already dropped Box")

  @throws(classOf[IllegalStateException])
  def swap[OtherOwner^](other: Box[T, OtherOwner]): Unit =
    (this.Impl, other.Impl) match
      case (Live[T, {Owner, this}](tag1, ref1), Live[T, OtherOwner](tag2, ref2)) =>
        this.Impl = Live(tag2, ref2).asInstanceOf[BoxImpl[T, {Owner, this}]]
        other.Impl = Live(tag1, ref1).asInstanceOf[BoxImpl[T, OtherOwner]]
      case _ =>
        throw new IllegalStateException(
          s"Cannot swap a ${this.Impl.name} with a ${other.Impl.name}"
        )

  @throws(classOf[IllegalStateException])
  def move[NewOwner^](): Box[T, NewOwner] =
    Impl match
      case Live[T, Owner](_, ref) =>
        val newBox = Box[T, NewOwner]()
        newBox.set(ref.unsafeGet())
        drop()
        newBox
      case Uninitialized[T, Owner]() =>
        throw new IllegalStateException("Cannot move an uninitialized Box")
      case Dropped[T, Owner]() =>
        throw new IllegalStateException("Cannot move a dropped Box")

  override def toString(): String = s"Box(${Impl})"
end Box

class OwnerOrigin:
  opaque type Key = Object
  def getKey(): Key = new Object
end OwnerOrigin

class BoxHolder[KeyType, T, Owner^](val box: Box[T, {Owner}]):
  def getBox(key: KeyType): Box[T, {Owner}] = box
end BoxHolder

object Box:
  def newFromBackground[T](value: T)(using ctx: Context^): Box[T, {ctx}]/*^{ctx}*/ =
    val ret = new Box[T, {ctx}]()
    ret.set(value)
    ret

  /**
    * TODO: Should make it private, then every time one wants to create a new `Box` has to somehow pass it a implicit
    * owner.
    */
  def newExplicit[T, Owner^](value: T): Box[T, Owner] =
    val ret = new Box[T, Owner]()
    ret.set(value)
    ret
end Box
