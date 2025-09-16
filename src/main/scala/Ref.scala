package imem

import language.experimental.captureChecking

trait Ref[T]:
  val parents: List[Ref[?]]

  def readCheck: Unit
  def writeCheck: Unit

class ImmutRef[T, Owner^](
    val tag: InternalRef[T]#Tag,
    val internalRef: InternalRef[T],
    override val parents: List[Ref[?]]
) extends Ref[T]:
  self: ImmutRef[T, Owner]^{Owner} =>

  def borrowImmut(using ctx: Context^): ImmutRef[T, {ctx, Owner}] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)

  def read[S, ctxOwner^, U >: T](readAction: Context^{ctxOwner} ?=> U => S)(using ctx: Context^{ctxOwner}): S =
    parents.foreach(_.readCheck)
    ctx.pushParent(this.asInstanceOf[Ref[T]])
    try
      internalRef.read(tag, readAction(using ctx))
    finally ctx.popParent()

  override def readCheck: Unit =
    internalRef.readCheck(tag)
  override def writeCheck: Unit =
    internalRef.useCheck(tag)
end ImmutRef

class MutRef[T, Owner^](
    val tag: InternalRef[T]#Tag,
    val internalRef: InternalRef[T],
    override val parents: List[Ref[?]]
) extends Ref[T]:
  self: MutRef[T, Owner]^{Owner} =>

  def borrowMut(using ctx: Context^): MutRef[T, {ctx, Owner}] =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParents)
  def borrowImmut(using ctx: Context^): ImmutRef[T, {ctx, Owner}] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)

  def read[S, ctxOwner^, U >: T](readAction: Context^{ctxOwner} ?=> U => S)(using ctx: Context^{ctxOwner}): S =
    parents.foreach(_.readCheck)
    // ?: Why should a `asInstanceOf` be necessary here?
    ctx.pushParent(this.asInstanceOf[Ref[T]])
    try
      internalRef.read(tag, readAction(using ctx))
    finally ctx.popParent()

  def write[S, ctxOwner^, U >: T](writeAction: Context^{ctxOwner} ?=> U => S)(using ctx: Context^{ctxOwner}): S =
    parents.foreach(_.writeCheck)
    ctx.pushParent(this.asInstanceOf[Ref[T]])
    try
      internalRef.write(tag, writeAction(using ctx))
    finally ctx.popParent()

  override def readCheck: Unit =
    internalRef.readCheck(tag)
  override def writeCheck: Unit =
    internalRef.useCheck(tag)
end MutRef
