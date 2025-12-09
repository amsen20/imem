package imem

import language.experimental.captureChecking

private[imem] class UnsafeRef[T](private val resource: T):
  private[imem] def applyAction[S](action: T => S): S = action(resource)

  private[imem] def applyActionWithLinearArg[S, LinearArgType <: scinear.Linear](
      linearArg: LinearArgType^,
      writeAction: (T, LinearArgType^{linearArg}) => S,
  ): S = writeAction(resource, linearArg)

  private[imem] def unsafeGet(): T = resource

  override def toString(): String = s"UnsafeRef(${resource})"
end UnsafeRef
