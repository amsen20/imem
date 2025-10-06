package imem

import language.experimental.captureChecking

/** TODO: Maybe make it an enum, and implement the internals in the Box methods.
  */
trait BoxImpl[T, +Owner^]:
  def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner]^{newOwner}
  def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner]^{newOwner}

  def name: String
  override def toString(): String

case class Uninitialized[T, +Owner^]() extends BoxImpl[T, Owner]:
  override def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner]^{newOwner} = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner]^{newOwner} = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def name: String = "Uninitialized"
  override def toString(): String = "Uninitialized"
end Uninitialized

case class Live[T, +Owner^](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]) extends BoxImpl[T, Owner]:
  override def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner]^{newOwner} =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)
  override def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner]^{newOwner} =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParents)
  override def name: String = "Live"
  override def toString(): String = s"Live(tag: ${tag}, internalRef: ${internalRef})"
end Live

case class Dropped[T, +Owner^]() extends BoxImpl[T, Owner]:
  override def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner]^{newOwner} = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner]^{newOwner} = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def name: String = "Dropped"
  override def toString(): String = "Dropped"
end Dropped

/**
 * TODO: For now, `Owner` can be any capability, and it may open ways to exploits.
 * Should restrict it to a specific type only.
*/
case class Box[T, @caps.use +Owner^]():
  /** Ensure `Box`captures the owner without storing it in a field.
  */
  // FIXME: For now, self types do not work for capture checking, to be specific, they work
  // for `this`, but they do not affect the capture set when the object is viewed from outside
  // , e.g., not in methods.
  self: Box[T, Owner]^{Owner} =>

  // TODO: Looks dirty for accessing the inner type. Should find a better way.
  var Impl: BoxImpl[T, {Owner, this}]^{Owner, this} = Uninitialized()

  @throws(classOf[IllegalStateException])
  def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner, this}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner]^{newOwner} = Impl.borrowImmut

  @throws(classOf[IllegalStateException])
  def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner, this}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner]^{newOwner} = Impl.borrowMut

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
  def swap[@caps.use OtherOwner^](other: Box[T, OtherOwner]^{OtherOwner}): Unit =
    (this.Impl, other.Impl) match
      case (Live[T, {Owner, this}](tag1, ref1), Live[T, OtherOwner](tag2, ref2)) =>
        this.Impl = Live(tag2, ref2).asInstanceOf[BoxImpl[T, {Owner, this}]]
        other.Impl = Live(tag1, ref1).asInstanceOf[BoxImpl[T, OtherOwner]]
      case _ =>
        throw new IllegalStateException(
          s"Cannot swap a ${this.Impl.name} with a ${other.Impl.name}"
        )

  @throws(classOf[IllegalStateException])
  def move[NewOwner^](): Box[T, NewOwner]^{NewOwner} =
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

class BoxHolder[KeyType, T, Owner^](val box: Box[T, {Owner}]^{Owner}):
  def getBox(key: KeyType): Box[T, {Owner}]^{Owner} = box
end BoxHolder

def readBox[T, Owner^, S, ctxOwner^, U >: T /* FIXME */](box: Box[T, Owner]^{Owner}, readAction: Context^{ctxOwner, Owner} ?=> U => S)(using ctx: Context^{ctxOwner}): S =
  val ref = box.borrowImmut[{ctx}, {ctx, Owner}](using ctx)
  read(ref, readAction(using ctx))(using ctx)

object Box:
  def newFromBackground[T](value: T)(using ctx: Context^): Box[T, {ctx}]^{ctx} =
    val ret = new Box[T, {ctx}]()
    ret.set(value)
    ret

  /**
    * TODO: Should make it private, then every time one wants to create a new `Box` has to somehow pass it a implicit
    * owner.
    */
  def newExplicit[T, Owner^](value: T): Box[T, Owner]^{Owner} =
    val ret = new Box[T, Owner]()
    ret.set(value)
    ret
end Box
