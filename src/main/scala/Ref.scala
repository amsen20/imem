package imem

import language.experimental.captureChecking

// TODO: Remove the union, instead store the internal refs and call check functions on them.
type Ref = ImmutRef[?, ?] | MutRef[?, ?]

class ImmutRef[T, +Owner^](
  val tag: InternalRef[T]#Tag,
  val internalRef: InternalRef[T],
  val parents: List[Ref]
)

def borrowImmut[@scinear.HideLinearity T, Owner^](
  self: ImmutRef[T, Owner]^
)(
  using ctx: Context^
): ImmutRef[T, {ctx, Owner}]^{self} =
  ImmutRef(self.internalRef.newSharedRef(self.tag), self.internalRef, ctx.getParents)

def read[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, ctxOwner^](
  self: ImmutRef[T, Owner]^,
  readAction: Context^{ctxOwner, Owner} ?=> T^ => S
)(
  using ctx: Context^{ctxOwner}
): S =
  self.parents.foreach(_ match
    case ref: ImmutRef[?, ?] => readCheck(ref)
    case ref: MutRef[?, ?] => readCheck(ref)
  )
  ctx.pushParent(self.asInstanceOf[Ref])

  try
    self.internalRef.read(self.tag, readAction(using ctx))
  finally
    ctx.popParent()

def readCheck[T, Owner^](self: ImmutRef[T, Owner]): Unit =
  self.internalRef.readCheck(self.tag)

def writeCheck[T, Owner^](self: ImmutRef[T, Owner]): Unit =
  self.internalRef.useCheck(self.tag)

class MutRef[T, +Owner^](
  val tag: InternalRef[T]#Tag,
  val internalRef: InternalRef[T],
  val parents: List[Ref]
) extends scinear.Linear

object MutRef:
  def unapply[T, Owner^](
    ref: MutRef[T, Owner]^
  ): Option[(InternalRef[T]#Tag, InternalRef[T], List[Ref])] =
    Some((ref.tag, ref.internalRef, ref.parents))
end MutRef

def borrowMut[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}](
  self: MutRef[T, Owner]^
)(
  using ctx: Context^{ctxOwner}
): (MutRef[T, newOwner], ValueHolder[newOwnerKey, MutRef[T, Owner]^{self}]) =
  val (tag, internalRef, parents) = MutRef.unapply(self).get
  (MutRef(internalRef.newMut(tag), internalRef, ctx.getParents), ValueHolder(MutRef(internalRef.newMut(tag), internalRef, parents)))

def borrowImmut[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}](
  self: MutRef[T, Owner]^
)(
  using ctx: Context^{ctxOwner}
): (ImmutRef[T, newOwner], ValueHolder[newOwnerKey, MutRef[T, Owner]^{self}]) =
  val (tag, internalRef, parents) = MutRef.unapply(self).get
  (ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents), ValueHolder(MutRef(internalRef.newMut(tag), internalRef, parents)))

def write[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, ctxOwner^](
  self: MutRef[T, Owner]^,
  writeAction: Context^{ctxOwner, Owner} ?=> T^ => S
)(
  using ctx: Context^{ctxOwner}
): S =
  val (tag, internalRef, parents) = MutRef.unapply(self).get

  parents.foreach(_ match
    case ref: ImmutRef[?, ?] => writeCheck(ref)
    case ref: MutRef[?, ?] => writeCheck(ref)
  )
  ctx.pushParent(MutRef(internalRef.newMut(tag), internalRef, parents).asInstanceOf[Ref])
  try
    internalRef.write(tag, writeAction(using ctx))
  finally
    ctx.popParent()

def writeWithLinearArg[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, ctxOwner^, LinearArgType <: scinear.Linear](
  self: MutRef[T, Owner]^,
  writeAction: Context^{ctxOwner, Owner} ?=> (T, LinearArgType) => S,
  linearArg: LinearArgType
)(
  using ctx: Context^{ctxOwner}
): S =
  val (tag, internalRef, parents) = MutRef.unapply(self).get

  parents.foreach(_ match
    case ref: ImmutRef[?, ?] => writeCheck(ref)
    case ref: MutRef[?, ?] => writeCheck(ref)
  )
  ctx.pushParent(MutRef(internalRef.newMut(tag), internalRef, parents).asInstanceOf[Ref])
  try
    internalRef.writeWithLinearArg(tag, writeAction(using ctx), linearArg)
  finally
    ctx.popParent()

def readCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  val (tag, internalRef, parents) = MutRef.unapply(self).get
  internalRef.readCheck(tag)

def writeCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  val (tag, internalRef, parents) = MutRef.unapply(self).get
  internalRef.useCheck(tag)
