package imem

import language.experimental.captureChecking

/** TODO: Maybe make it an enum, and implement the internals in the Box methods.
  */
trait BoxImpl[T]:
  def borrowImmut(using Context): ImmutRef[T]
  def borrowMut(using Context): MutRef[T]

  def name: String
  override def toString(): String

case class Uninitialized[T]() extends BoxImpl[T]:
  override def borrowImmut(using Context): ImmutRef[T] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def borrowMut(using Context): MutRef[T] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def name: String = "Uninitialized"
  override def toString(): String = "Uninitialized"
end Uninitialized

case class Live[T](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]) extends BoxImpl[T]:
  override def borrowImmut(using ctx: Context): ImmutRef[T] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)
  override def borrowMut(using ctx: Context): MutRef[T] =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParents)
  override def name: String = "Live"
  override def toString(): String = s"Live(tag: ${tag}, internalRef: ${internalRef})"
end Live

case class Dropped[T]() extends BoxImpl[T]:
  override def borrowImmut(using Context): ImmutRef[T] = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def borrowMut(using Context): MutRef[T] = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def name: String = "Dropped"
  override def toString(): String = "Dropped"
end Dropped

case class Box[T, Owner^]():
  /** Ensure `Box`captures the owner without storing it in a field.
  */
  self: Box[T, Owner]^{Owner} =>

  // TODO: Looks dirty for accessing the inner type. Should find a better way.
  var Impl: BoxImpl[T] = Uninitialized()

  @throws(classOf[IllegalStateException])
  def borrowImmut(using Context): ImmutRef[T] = Impl.borrowImmut

  @throws(classOf[IllegalStateException])
  def borrowMut(using Context): MutRef[T] = Impl.borrowMut

  @throws(classOf[IllegalStateException])
  def set(value: T): Unit =
    Impl match
      case Uninitialized() =>
        val (ref, tag) = InternalRef.newWithTag(value)
        Impl = Live(tag, ref)
      case Live(_, _) =>
        drop()
        val (ref, tag) = InternalRef.newWithTag(value)
        Impl = Live(tag, ref)
      case Dropped() =>
        throw new IllegalStateException("Cannot set a value to a dropped Box")

  @throws(classOf[IllegalStateException])
  def drop(): Unit =
    Impl match
      case Uninitialized() =>
        throw new IllegalStateException("Cannot drop an uninitialized Box")
      case Live(_, ref) =>
        ref.drop()
        Impl = Dropped()
      case Dropped() =>
        throw new IllegalStateException("Cannot drop an already dropped Box")

  @throws(classOf[IllegalStateException])
  def swap[OtherOwner^](other: Box[T, OtherOwner]): Unit =
    (this.Impl, other.Impl) match
      case (Live(tag1, ref1), Live(tag2, ref2)) =>
        this.Impl = Live(tag2, ref2)
        other.Impl = Live(tag1, ref1)
      case _ =>
        throw new IllegalStateException(
          s"Cannot swap a ${this.Impl.name} with a ${other.Impl.name}"
        )

  @throws(classOf[IllegalStateException])
  def move[NewOwner^](): Box[T, NewOwner] =
    Impl match
      case Live(_, ref) =>
        val newBox = Box[T, NewOwner]()
        newBox.set(ref.unsafeGet())
        drop()
        newBox
      case Uninitialized() =>
        throw new IllegalStateException("Cannot move an uninitialized Box")
      case Dropped() =>
        throw new IllegalStateException("Cannot move a dropped Box")

  override def toString(): String = s"Box(${Impl})"
end Box

object Box:
  def newFromBackground[T, Owner^](value: T)(using OwnerCarrier^{Owner}): Box[T, Owner] =
    val ret = new Box[T, Owner]()
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
