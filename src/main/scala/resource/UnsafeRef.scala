package imem.resource

import language.experimental.captureChecking

class UnsafeRef[T](val value: T):
  def read[S](readAction: T => S): S = readAction(value)

  /** TODO: This should be exclusive mutable capability.
    */
  def modify[S](writeAction: T => S): S = writeAction(value)

  /** TODO: Should be private, it is used for implementing moving
    */
  def unsafeGet(): T = value

  override def toString(): String = s"UnsafeRef(${value})"
end UnsafeRef
