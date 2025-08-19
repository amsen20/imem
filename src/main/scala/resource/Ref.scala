package imem.resource

import scala.compiletime.ops.int

trait Ref[T]:
  val parent: Option[Ref[?]]

  def readCheck: Unit
  def writeCheck: Unit

class ImmutRef[T](
    val tag: InternalRef[T]#Tag,
    val internalRef: InternalRef[T],
    override val parent: Option[Ref[?]]
) extends Ref[T]:
  def borrowImmut(using ctx: Context): ImmutRef[T] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParent)

  def read[S](readAction: T => S)(using ctx: Context): S =
    readCheck
    val currentParent = ctx.getParent
    ctx.setParent(Some(this))
    val res = internalRef.read(tag, readAction)
    ctx.setParent(currentParent)
    res

  override def readCheck: Unit =
    parent.map(_.readCheck)
    internalRef.readCheck(tag)
  override def writeCheck: Unit =
    parent.map(_.writeCheck)
    internalRef.useCheck(tag)
end ImmutRef

class MutRef[T](
    val tag: InternalRef[T]#Tag,
    val internalRef: InternalRef[T],
    override val parent: Option[Ref[?]]
) extends Ref[T]:
  def borrowMut(using ctx: Context): MutRef[T] =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParent)
  def borrowImmut(using ctx: Context): ImmutRef[T] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParent)

  def read[S](readAction: T => S)(using ctx: Context): S =
    readCheck
    val currentParent = ctx.getParent
    ctx.setParent(Some(this))
    val res = internalRef.read(tag, readAction)
    ctx.setParent(currentParent)
    res

  def write[S](writeAction: T => S)(using ctx: Context): S =
    writeCheck
    val currentParent = ctx.getParent
    ctx.setParent(Some(this))
    val res = internalRef.write(tag, writeAction)
    ctx.setParent(currentParent)
    res

  override def readCheck: Unit =
    parent.map(_.readCheck)
    internalRef.readCheck(tag)
  override def writeCheck: Unit =
    parent.map(_.writeCheck)
    internalRef.useCheck(tag)
end MutRef
