package imem.resource

class ImmutRef[T](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]):
  def borrowImmut: ImmutRef[T] = internalRef.newSharedRef(tag)

  def read[S](readAction: T => S): S = internalRef.read(tag, readAction)
end ImmutRef

class MutRef[T](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]):
  def borrowMut: MutRef[T] = internalRef.newMut(tag)
  def borrowImmut: ImmutRef[T] = internalRef.newSharedRef(tag)

  def read[S](readAction: T => S): S = internalRef.read(tag, readAction)
  def write[S](writeAction: T => S): S = internalRef.write(tag, writeAction)
end MutRef
