package imem.resource

/** TODO: Maybe make it an enum, and implement the internals in the Box methods.
  */
trait BoxImpl[T]:
  def borrowImmut(using Context): ImmutRef[T] = ???
  def borrowMut(using Context): MutRef[T] = ???

  def name: String = ???

case class Uninitialized[T]() extends BoxImpl[T]:
  override def borrowImmut(using Context): ImmutRef[T] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def borrowMut(using Context): MutRef[T] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def name: String = "Uninitialized"
end Uninitialized

case class Live[T](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]) extends BoxImpl[T]:
  override def borrowImmut(using ctx: Context): ImmutRef[T] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParent)
  override def borrowMut(using ctx: Context): MutRef[T] =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParent)
end Live

case class Dropped[T]() extends BoxImpl[T]:
  override def borrowImmut(using Context): ImmutRef[T] = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def borrowMut(using Context): MutRef[T] = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def name: String = "Dropped"
end Dropped

case class Box[T]():

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
  def swap(other: Box[T]): Unit =
    (this.Impl, other.Impl) match
      case (Live(tag1, ref1), Live(tag2, ref2)) =>
        this.Impl = Live(tag2, ref2)
        other.Impl = Live(tag1, ref1)
      case _ =>
        throw new IllegalStateException(
          s"Cannot swap a ${this.Impl.name} with a ${other.Impl.name}"
        )

  @throws(classOf[IllegalStateException])
  def move(): Box[T] =
    Impl match
      case Live(_, ref) =>
        val newBox = Box[T]()
        newBox.set(ref.unsafeGet())
        drop()
        newBox
      case Uninitialized() =>
        throw new IllegalStateException("Cannot move an uninitialized Box")
      case Dropped() =>
        throw new IllegalStateException("Cannot move a dropped Box")
end Box

object Box:
  def apply[T](value: T): Box[T] =
    val ret = Box[T]()
    ret.set(value)
    ret
end Box
