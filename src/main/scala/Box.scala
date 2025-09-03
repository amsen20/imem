package imem

import language.experimental.captureChecking

/** TODO: Maybe make it an enum, and implement the internals in the Box methods.
  */
trait BoxImpl[T, Owner^]:
  def borrowImmut(using Context): ImmutRef[T, Owner]
  def borrowMut(using Context): MutRef[T, Owner]

  def name: String
  override def toString(): String

case class Uninitialized[T, Owner^]() extends BoxImpl[T, Owner]:
  override def borrowImmut(using Context): ImmutRef[T, Owner] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def borrowMut(using Context): MutRef[T, Owner] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def name: String = "Uninitialized"
  override def toString(): String = "Uninitialized"
end Uninitialized

case class Live[T, Owner^](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]) extends BoxImpl[T, Owner]:
  override def borrowImmut(using ctx: Context): ImmutRef[T, Owner] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)
  override def borrowMut(using ctx: Context): MutRef[T, Owner] =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParents)
  override def name: String = "Live"
  override def toString(): String = s"Live(tag: ${tag}, internalRef: ${internalRef})"
end Live

case class Dropped[T, Owner^]() extends BoxImpl[T, Owner]:
  override def borrowImmut(using Context): ImmutRef[T, Owner] = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def borrowMut(using Context): MutRef[T, Owner] = throw new IllegalStateException(
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
  var Impl: BoxImpl[T, {this}] = Uninitialized()

  @throws(classOf[IllegalStateException])
  def borrowImmut(using Context): ImmutRef[T, {this}] = Impl.borrowImmut

  @throws(classOf[IllegalStateException])
  def borrowMut(using Context): MutRef[T, {this}] = Impl.borrowMut

  @throws(classOf[IllegalStateException])
  def set(value: T): Unit =
    Impl match
      // ?: why should I mentioned [T, {this}] every time?
      // ?: Also, why is `asInstanceOf[BoxImpl[T, {this}]]` needed?
      case Uninitialized[T, {this}]() =>
        val (ref, tag) = InternalRef.newWithTag(value)
        Impl = Live[T, {this}](tag, ref).asInstanceOf[BoxImpl[T, {this}]]
      case Live[T, {this}](_, _) =>
        drop()
        val (ref, tag) = InternalRef.newWithTag(value)
        Impl = Live(tag, ref).asInstanceOf[BoxImpl[T, {this}]]
      case Dropped[T, {this}]() =>
        throw new IllegalStateException("Cannot set a value to a dropped Box")

  @throws(classOf[IllegalStateException])
  def drop(): Unit =
    Impl match
      case Uninitialized[T, {this}]() =>
        throw new IllegalStateException("Cannot drop an uninitialized Box")
      case Live[T, {this}](_, ref) =>
        ref.drop()
        Impl = Dropped()
      case Dropped[T, {this}]() =>
        throw new IllegalStateException("Cannot drop an already dropped Box")

  @throws(classOf[IllegalStateException])
  def swap[OtherOwner^](other: Box[T, OtherOwner]^{OtherOwner}): Unit =
    (this.Impl, other.Impl) match
      case (Live[T, {this}](tag1, ref1), Live[T, {other}](tag2, ref2)) =>
        this.Impl = Live(tag2, ref2).asInstanceOf[BoxImpl[T, {this}]]
        other.Impl = Live(tag1, ref1).asInstanceOf[BoxImpl[T, {other}]]
      case _ =>
        throw new IllegalStateException(
          s"Cannot swap a ${this.Impl.name} with a ${other.Impl.name}"
        )

  @throws(classOf[IllegalStateException])
  def move[NewOwner^](): Box[T, NewOwner] =
    Impl match
      case Live[T, {this}](_, ref) =>
        val newBox = Box[T, NewOwner]()
        newBox.set(ref.unsafeGet())
        drop()
        newBox
      case Uninitialized[T, {this}]() =>
        throw new IllegalStateException("Cannot move an uninitialized Box")
      case Dropped[T, {this}]() =>
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
