package imem.resource

/** `Context` is used as a temporary approach to enforce some borrowing, ownership, and mutability
  * rules. The goal is to make these rules enforcements in compile-time but at first, we try to make
  * them happen in runtime.
  *
  * NOTE: This is a temporary solution and will be removed/replaced throughout time.
  */
trait Context:
  def getParent: Option[Ref[?]]
  def setParent(parent: Option[Ref[?]]): Unit
end Context

class DefaultContext extends Context:
  private var parent: Option[Ref[?]] = None
  override def getParent: Option[Ref[?]] = parent
  override def setParent(newParent: Option[Ref[?]]): Unit = parent = newParent
end DefaultContext
