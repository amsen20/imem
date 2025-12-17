package imem

import language.experimental.captureChecking

class Context[WriteCap^]

def withOwnership[T](block: [@caps.use WriteCap^] => Context[WriteCap]^ => T): T =
  object writeCap extends caps.Capability
  Object().asInstanceOf[T]
  val ctx = Context[{writeCap}]()
  block[{writeCap}](ctx)
