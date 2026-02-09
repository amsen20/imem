package imem

import language.experimental.captureChecking

class Lifetime[OwnersInput^] extends scinear.Linear, caps.Capability:
  type Owners^ = {OwnersInput, this}
  opaque type Key = Object

  def getKey[O^](): Key = Object()
end Lifetime

class ValueHolder[KeyType, T](private[imem] val value: T) extends scinear.Linear

def unlockHolder[KeyType, @scinear.HideLinearity T](
  key: KeyType,
  holder: ValueHolder[KeyType, T]^
): T^{holder} = holder.value

opaque type NeverUsableKey = Object
