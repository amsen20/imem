package imem

import language.experimental.captureChecking

type Ref = ImmutRef[?, ?] | MutRef[?, ?]

class ImmutRef[T, +Owner^](
    val tag: InternalRef[T]#Tag,
    val internalRef: InternalRef[T],
    val parents: List[Ref]
):
  // FIXME: For now, self types do not work for capture checking, to be specific, they work
  // for `this`, but they do not affect the capture set when the object is viewed from outside
  // , e.g., not in methods.
  self: ImmutRef[T, Owner]^{Owner} =>

  def borrowImmut(using ctx: Context^): ImmutRef[T, {ctx, Owner}]^{ctx, Owner} =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)

  def read[S, ctxOwner^, U >: T](readAction: Context^{ctxOwner, Owner} ?=> U => S)(using ctx: Context^{ctxOwner}): S =
    parents.foreach(_ match
      case ref: ImmutRef[?, ?] => ref.readCheck
      case ref: MutRef[?, ?] => ref.readCheck
    )
    ctx.pushParent(this.asInstanceOf[Ref])
    try
      internalRef.read(tag, readAction(using ctx))
    finally ctx.popParent()

  def readCheck: Unit =
    internalRef.readCheck(tag)
  def writeCheck: Unit =
    internalRef.useCheck(tag)
end ImmutRef

class MutRef[T, +Owner^](
    val tag: InternalRef[T]#Tag,
    val internalRef: InternalRef[T],
    val parents: List[Ref]
):
  // FIXME: For now, self types do not work for capture checking, to be specific, they work
  // for `this`, but they do not affect the capture set when the object is viewed from outside
  // , e.g., not in methods.
  self: MutRef[T, Owner]^{Owner} =>

  def borrowMut(using ctx: Context^): MutRef[T, {ctx, Owner}]^{ctx, Owner} =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParents)
  def borrowImmut(using ctx: Context^): ImmutRef[T, {ctx, Owner}]^{ctx, Owner} =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)

  def read[S, ctxOwner^, U >: T](readAction: Context^{ctxOwner, Owner} ?=> U => S)(using ctx: Context^{ctxOwner}): S =
    parents.foreach(_ match
      case ref: ImmutRef[?, ?] => ref.readCheck
      case ref: MutRef[?, ?] => ref.readCheck
    )
    ctx.pushParent(this.asInstanceOf[Ref])
    try
      internalRef.read(tag, readAction(using ctx))
    finally ctx.popParent()

  def write[S, ctxOwner^, U >: T](writeAction: Context^{ctxOwner, Owner} ?=> U => S)(using ctx: Context^{ctxOwner}): S =
    parents.foreach(_ match
      case ref: ImmutRef[?, ?] => ref.readCheck
      case ref: MutRef[?, ?] => ref.writeCheck
    )
    ctx.pushParent(this.asInstanceOf[Ref])
    try
      internalRef.write(tag, writeAction(using ctx))
    finally ctx.popParent()

  def readCheck: Unit =
    internalRef.readCheck(tag)
  def writeCheck: Unit =
    internalRef.useCheck(tag)
end MutRef
