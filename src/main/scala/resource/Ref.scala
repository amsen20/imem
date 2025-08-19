package imem.resource

class ImmutRef[T](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]):
  def borrowImmut(using Context): ImmutRef[T] = internalRef.newSharedRef(tag)

  def read[S](readAction: T => S)(using Context): S = internalRef.read(tag, readAction)
end ImmutRef

class MutRef[T](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]):
  def borrowMut(using Context): MutRef[T] = internalRef.newMut(tag)
  def borrowImmut(using Context): ImmutRef[T] = internalRef.newSharedRef(tag)

  def read[S](readAction: T => S)(using Context): S = internalRef.read(tag, readAction)
  def write[S](writeAction: T => S)(using Context): S = internalRef.write(tag, writeAction)
end MutRef
