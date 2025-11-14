package imem

import language.experimental.captureChecking

class Lifetime[OwnersInput^] extends scinear.Linear, caps.Capability:
  type Owners^ = {OwnersInput, this}
  opaque type Key = Object

  def getKey[O^](): Key =
    new Object
end Lifetime

object Lifetime:
  def unapply[O^](lf: Lifetime[O^]): Option[lf.Key] =
    Some(lf.getKey())
end Lifetime

class ValueHolder[KeyType, T](val value: T) extends scinear.Linear

// TODO: rename it to `getBox`, `endBorrow`? find a better name.
def accessValue[KeyType, @scinear.HideLinearity T](
  key: KeyType,
  holder: ValueHolder[KeyType, T]
): T =
  holder.value
