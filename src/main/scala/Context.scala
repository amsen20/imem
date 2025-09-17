package imem

import language.experimental.captureChecking

/** `Context` is used as a temporary approach to enforce some borrowing, ownership, and mutability
  * rules. The goal is to make these rules enforcements in compile-time but at first, we try to make
  * them happen in runtime.
  *
  * NOTE: This is a temporary solution and will be removed/replaced throughout time.
  */
trait Context:
  def getParents: List[Ref[?]]
  def pushParent(parent: Ref[?]): Unit
  def popParent(): Unit
end Context

class DefaultContext extends Context:
  private var parents: List[Ref[?]] = List.empty
  override def getParents: List[Ref[?]] = parents.toList
  override def pushParent(parent: Ref[?]): Unit =
    parents = parent :: parents
  override def popParent(): Unit = parents match
    case Nil       => throw new IllegalStateException("No parent to pop")
    case _ :: tail => parents = tail
end DefaultContext

def withOwnership[T](block: Context^ => T): T =
  val ctx = new imem.DefaultContext
  block(ctx)
