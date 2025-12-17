package imem

import language.experimental.captureChecking

class ImmutRef[T, Owner^](
  private[imem] val tag: InternalRef[T]#Tag,
  private[imem] val internalRef: InternalRef[T]
)

def borrowImmut[@scinear.HideLinearity T, Owner^, newOwner^ >: {Owner}, WriteCap^](
  self: ImmutRef[T, Owner]^
)(
  using ctx: Context[WriteCap]
  // TODO: Check if not capturing `self` in return type is ok.
): ImmutRef[T, {Owner}] =
  ImmutRef(self.internalRef.newShared(self.tag), self.internalRef)

def read[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, WriteCap^](
  self: ImmutRef[T, Owner]^,
  readAction: Context[WriteCap] ?-> T^ ->{Owner} S
)(
  using ctx: Context[WriteCap]
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

def borrowMut[@scinear.HideLinearity T, Owner^, newOwnerKey, newOwner^ >: {Owner}, @caps.use WriteCap^](
  self: MutRef[T, Owner]^
)(
  using ctx: Context[WriteCap]
): (MutRef[T, newOwner], ValueHolder[newOwnerKey, MutRef[T, Owner]^{self}]) =
  val (tag, internalRef) = MutRef.unapply(self).get
  (MutRef(internalRef.newUnique(tag), internalRef), ValueHolder(MutRef(tag, internalRef)))

def borrowImmut[@scinear.HideLinearity T, Owner^, newOwnerKey, newOwner^ >: {Owner}, WriteCap^](
  self: MutRef[T, Owner]^
)(
  using ctx: Context[WriteCap]
): (ImmutRef[T, newOwner], ValueHolder[newOwnerKey, MutRef[T, Owner]^{self}]) =
  val (tag, internalRef) = MutRef.unapply(self).get
  (ImmutRef(internalRef.newShared(tag), internalRef), ValueHolder(MutRef(tag, internalRef)))

def write[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, @caps.use WriteCap^](
  self: MutRef[T, Owner]^,
  writeAction: Context[{WriteCap}] ?->{WriteCap} T^ ->{Owner, WriteCap} S
)(
  using ctx: Context[WriteCap]
): S =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.write(tag, writeAction(using ctx))

def writeWithLinearArg[@scinear.HideLinearity T, Owner^, @scinear.HideLinearity S, LinearArgType <: scinear.Linear, @caps.use WriteCap^](
  self: MutRef[T, Owner]^,
  linearArg: LinearArgType^,
  writeAction: Context[WriteCap] ?->{Owner, WriteCap} (T, LinearArgType^{linearArg}) ->{Owner, WriteCap} S,
)(
  using ctx: Context[WriteCap]
): S =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.writeWithLinearArg[S, LinearArgType](tag, linearArg, writeAction(using ctx))

private[imem] def readCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.readCheck(tag)

private[imem] def writeCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  val (tag, internalRef) = MutRef.unapply(self).get
  internalRef.useCheck(tag)
