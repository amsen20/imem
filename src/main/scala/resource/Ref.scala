package imem.resource

import scala.compiletime.ops.int

trait Ref[T]:
  val parents: List[Ref[?]]

  def readCheck: Unit
  def writeCheck: Unit

class ImmutRef[T](
    val tag: InternalRef[T]#Tag,
    val internalRef: InternalRef[T],
    override val parents: List[Ref[?]]
) extends Ref[T]:
  def borrowImmut(using ctx: Context): ImmutRef[T] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)

  def read[S](readAction: T => S)(using ctx: Context): S =
    parents.foreach(_.readCheck)
    ctx.pushParent(this)
    try
      internalRef.read(tag, readAction)
    finally ctx.popParent()

  override def readCheck: Unit =
    internalRef.readCheck(tag)
  override def writeCheck: Unit =
    internalRef.useCheck(tag)
end ImmutRef

class MutRef[T](
    val tag: InternalRef[T]#Tag,
    val internalRef: InternalRef[T],
    override val parents: List[Ref[?]]
) extends Ref[T]:
  def borrowMut(using ctx: Context): MutRef[T] =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParents)
  def borrowImmut(using ctx: Context): ImmutRef[T] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)

  def read[S](readAction: T => S)(using ctx: Context): S =
    parents.foreach(_.readCheck)
    ctx.pushParent(this)
    try
      internalRef.read(tag, readAction)
    finally ctx.popParent()

  def write[S](writeAction: T => S)(using ctx: Context): S =
    parents.foreach(_.writeCheck)
    ctx.pushParent(this)
    try
      internalRef.write(tag, writeAction)
    finally ctx.popParent()

  override def readCheck: Unit =
    internalRef.readCheck(tag)
  override def writeCheck: Unit =
    internalRef.useCheck(tag)
end MutRef
