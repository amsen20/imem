package imem

import language.experimental.captureChecking

/*
  * TODO: Check if capturing the aggregated owners would not cause a problem.
  * The other option is to aggregate them through a type parameter in the Context class.
  * But the other option didn't work with Scinear due to mentioning lifetimes in nested scopes.
  * The other option is preferable because it does not allow the user to change the owners due to
  * covariance or contravariant.
  */
class Context[WC^, MC^] private[imem] ()

def withImem[T](block: [@caps.use WC^, MC^] => Context[WC, MC]^ => T): T =
  object WC extends caps.Capability
  object MC extends caps.Capability
  val ctx = Context[{WC}, {MC}]()
  block[{WC}, {MC}](ctx)
