package imem

import language.experimental.captureChecking

type Ref = ImmutRef[?, ?] | MutRef[?, ?]

class ImmutRef[T, +Owner^](
    val tag: InternalRef[T]#Tag,
    val internalRef: InternalRef[T],
    val parents: List[Ref]
)

def borrowImmut[T, Owner^](self: ImmutRef[T, Owner]^{Owner})(using ctx: Context^): ImmutRef[T, {ctx, Owner}]^{ctx, Owner} =
  ImmutRef(self.internalRef.newSharedRef(self.tag), self.internalRef, ctx.getParents)

def read[T, Owner^, S, ctxOwner^, U >: T /* FIXME: Is this `U` needed anymore?, I maybe able to delete it */](self: ImmutRef[T, Owner]^{Owner}, readAction: Context^{ctxOwner, Owner} ?=> U => S)(using ctx: Context^{ctxOwner}): S =
  self.parents.foreach(_ match
    case ref: ImmutRef[?, ?] => readCheck(ref)
    case ref: MutRef[?, ?] => ref.readCheck
  )
  ctx.pushParent(self.asInstanceOf[Ref])
  try
    self.internalRef.read(self.tag, readAction(using ctx))
  finally ctx.popParent()

def readCheck[T, Owner^](self: ImmutRef[T, Owner]^{Owner}): Unit =
  self.internalRef.readCheck(self.tag)

def writeCheck[T, Owner^](self: ImmutRef[T, Owner]^{Owner}): Unit =
  self.internalRef.useCheck(self.tag)

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
      case ref: ImmutRef[?, ?] => imem.readCheck(ref)
      case ref: MutRef[?, ?] => ref.readCheck
    )
    ctx.pushParent(this.asInstanceOf[Ref])
    try
      internalRef.read(tag, readAction(using ctx))
    finally ctx.popParent()

  def write[S, ctxOwner^, U >: T](writeAction: Context^{ctxOwner, Owner} ?=> U => S)(using ctx: Context^{ctxOwner}): S =
    parents.foreach(_ match
      case ref: ImmutRef[?, ?] => imem.writeCheck(ref)
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
