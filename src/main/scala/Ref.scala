package imem

import language.experimental.captureChecking

// TODO: Remove the union, instead store the internal refs and call check functions on them.
type Ref = ImmutRef[?, ?] | MutRef[?, ?]

class ImmutRef[T, Owner^](
  private[imem] val tag: InternalRef[T]#Tag,
  private[imem] val internalRef: InternalRef[T]
)

def borrowImmut[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}, WriteCap^](
  self: ImmutRef[T, Owner]^
)(
  using ctx: Context[WriteCap]^{ctxOwner}
  // TODO: Check if not capturing `self` in return type is ok.
): ImmutRef[T, newOwner] =
  ImmutRef(self.internalRef.newShared(self.tag), self.internalRef)

def read[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, ctxOwner^, WriteCap^](
  self: ImmutRef[T, Owner]^,
  readAction: Context[WriteCap]^{ctxOwner, Owner} ?-> T^ ->{Owner, ctxOwner} S
)(
  using ctx: Context[WriteCap]^{ctxOwner}
): S =
  self.internalRef.read(self.tag, readAction(using ctx))

private[imem] def readCheck[T, Owner^](self: ImmutRef[T, Owner]): Unit =
  self.internalRef.readCheck(self.tag)

private[imem] def writeCheck[T, Owner^](self: ImmutRef[T, Owner]): Unit =
  self.internalRef.useCheck(self.tag)

class MutRef[T, Owner^](
  private[imem] val tag: InternalRef[T]#Tag,
  private[imem] val internalRef: InternalRef[T]
) extends scinear.Linear

private[imem] object MutRef:
  private[imem] def unapply[T, Owner^](
    ref: MutRef[T, Owner]^
  ): Option[(InternalRef[T]#Tag, InternalRef[T])] =
    Some((ref.tag, ref.internalRef))
end MutRef

def borrowMut[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}, @caps.use WriteCap^](
  self: MutRef[T, Owner]^
)(
  using ctx: Context[WriteCap]^{ctxOwner}
): (MutRef[T, newOwner], ValueHolder[newOwnerKey, MutRef[T, Owner]^{self}]) =
  val (tag, internalRef) = MutRef.unapply(self).get
  (MutRef(internalRef.newUnique(tag), internalRef), ValueHolder(MutRef(tag, internalRef)))

def borrowImmut[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}, WriteCap^](
  self: MutRef[T, Owner]^
)(
  using ctx: Context[WriteCap]^{ctxOwner}
): (ImmutRef[T, newOwner], ValueHolder[newOwnerKey, MutRef[T, Owner]^{self}]) =
  val (tag, internalRef) = MutRef.unapply(self).get
  (ImmutRef(internalRef.newShared(tag), internalRef), ValueHolder(MutRef(tag, internalRef)))

def write[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, ctxOwner^, @caps.use WriteCap^](
  self: MutRef[T, Owner]^,
  writeAction: Context[{WriteCap}]^{ctxOwner, Owner} ?->{WriteCap} T^ ->{Owner, ctxOwner, WriteCap} S
)(
  using ctx: Context[WriteCap]^{ctxOwner}
): S =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.write(tag, writeAction(using ctx))

def writeWithLinearArg[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, ctxOwner^, LinearArgType <: scinear.Linear, @caps.use WriteCap^](
  self: MutRef[T, Owner]^,
  linearArg: LinearArgType^,
  writeAction: Context[WriteCap]^{ctxOwner, Owner} ?->{Owner, ctxOwner, WriteCap} (T, LinearArgType^{linearArg}) ->{Owner, ctxOwner, WriteCap} S,
)(
  using ctx: Context[WriteCap]^{ctxOwner}
): S =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.writeWithLinearArg[S, LinearArgType](tag, linearArg, writeAction(using ctx))

private[imem] def readCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  // FIXME: It is possible that the parents are not sufficient to guarantee safety.
  // This means that this function has to call checks on parents as well.
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.readCheck(tag)

private[imem] def writeCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.useCheck(tag)
