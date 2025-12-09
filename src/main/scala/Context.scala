package imem

import language.experimental.captureChecking

/** `Context` is used as a temporary approach to enforce some borrowing, ownership, and mutability
  * rules. The goal is to make these rules enforcements in compile-time but at first, we try to make
  * them happen in runtime.
  *
  * NOTE: This is a temporary solution and will be removed/replaced throughout time.
  */
class Context[WriteCap^]:
  private var UnderOpRefs: List[Ref] = List.empty
  def getUnderOpRefs: List[Ref] = UnderOpRefs.toList
  def push(underOp: Ref): Unit =
    UnderOpRefs = underOp :: UnderOpRefs
  def pop(): Unit = UnderOpRefs match
    case Nil       => throw new IllegalStateException("No under operation reference to pop")
    case _ :: tail => UnderOpRefs = tail
end Context

def withOwnership[T](block: [@caps.use WriteCap^] => Context[WriteCap]^ => T): T =
  object writeCap extends caps.Capability
  Object().asInstanceOf[T]
  val ctx = Context[{writeCap}]()
  block[{writeCap}](ctx)

class MovingContext[Owner^]