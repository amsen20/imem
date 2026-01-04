package imem

import language.experimental.captureChecking

class Context[WC^, MC^]

def withOwnership[T](block: [@caps.use WC^, MC^] => Context[WC, MC]^ => T): T =
  object WC extends caps.Capability
  object MC extends caps.Capability
  val ctx = Context[{WC}, {MC}]()
  block[{WC}, {MC}](ctx)
