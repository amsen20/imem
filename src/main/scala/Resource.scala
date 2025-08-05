package imem

class Box[T](private val value: T) {
  def borrowImmut: ImmutRef[T] = ImmutRef(value)
  def borrowMut: MutRef[T] = MutRef(value)
}

class ImmutRef[T](val value: T) {
  def getValue: T = value
}

// TODO I couldn't find a way to show what in `MutRef` value is mutable and in `ImmutRef` is not.
class MutRef[T](override val value: T) extends ImmutRef[T](value)
