package imem

import language.experimental.captureChecking

class ImmutRef[T, Owner^](
  private[imem] val tag: InternalRef[T]#Tag,
  private[imem] val internalRef: InternalRef[T]
)

def borrowImmut[@scinear.HideLinearity T, Owner^, ctxOwner^, WC^, MC^]( // Inferrable
)[newOwnerKey, newOwner^ >: {ctxOwner, Owner}]( // Non-inferrable
  self: ImmutRef[T, Owner]^
)(
  using ctx: Context[WC, MC]^{ctxOwner}
): ImmutRef[T, newOwner] =
  ImmutRef(self.internalRef.newShared(self.tag), self.internalRef)

def read[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, ctxOwner^, WC^, MC^](
  self: ImmutRef[T, Owner]^,
  readAction: Context[WC, MC]^{ctxOwner, Owner} ?-> T^ ->{Owner, ctxOwner} S
)(
  using ctx: Context[WC, MC]^{ctxOwner}
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

def borrowMut[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}, @caps.use WC^, MC^](
  self: MutRef[T, Owner]^
)(
  using ctx: Context[WC, MC]^{ctxOwner}
): (MutRef[T, newOwner], ValueHolder[newOwnerKey, MutRef[T, Owner]^{self}]) =
  val (tag, internalRef) = MutRef.unapply(self).get
  (MutRef(internalRef.newUnique(tag), internalRef), ValueHolder(MutRef(tag, internalRef)))

def borrowImmut[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}, WC^, MC^](
  self: MutRef[T, Owner]^
)(
  using ctx: Context[WC, MC]^{ctxOwner}
): (ImmutRef[T, newOwner], ValueHolder[newOwnerKey, MutRef[T, Owner]^{self}]) =
  val (tag, internalRef) = MutRef.unapply(self).get
  (ImmutRef(internalRef.newShared(tag), internalRef), ValueHolder(MutRef(tag, internalRef)))

def write[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, ctxOwner^, @caps.use WC^, MC^](
  self: MutRef[T, Owner]^,
  writeAction: Context[{WC}, {MC}]^{ctxOwner, Owner} ?->{WC} T^ ->{Owner, ctxOwner, WC} S
)(
  using ctx: Context[WC, MC]^{ctxOwner}
): S =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.write(tag, writeAction(using ctx))

def writeWithLinearArg[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, ctxOwner^, LinearArgType <: scinear.Linear, @caps.use WC^, MC^](
  self: MutRef[T, Owner]^,
  linearArg: LinearArgType^,
  writeAction: Context[WC, MC]^{ctxOwner, Owner} ?->{Owner, ctxOwner, WC} (T, LinearArgType^{linearArg}) ->{Owner, ctxOwner, WC} S,
)(
  using ctx: Context[WC, MC]^{ctxOwner}
): S =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.writeWithLinearArg[S, LinearArgType](tag, linearArg, writeAction(using ctx))

private[imem] def readCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.readCheck(tag)

private[imem] def writeCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.useCheck(tag)
