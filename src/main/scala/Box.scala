package imem

import language.experimental.captureChecking

private[imem] def borrowImmutInternal[T, Owner^, ctxOwner^, newOwner^ >: {ctxOwner, Owner}, WC^, MC^](
  tag: InternalRef[T]#Tag,
  internalRef: InternalRef[T]
)(using ctx: Context[WC, MC]^{ctxOwner}): ImmutRef[T, newOwner] =
  ImmutRef(internalRef.newShared(tag), internalRef)

private[imem] def borrowMutInternal[T, Owner^, ctxOwner^, newOwner^ >: {ctxOwner, Owner}, WC^, MC^](
  tag: InternalRef[T]#Tag,
  internalRef: InternalRef[T]
)(using ctx: Context[WC, MC]^{ctxOwner}): MutRef[T, newOwner] =
  MutRef(internalRef.newUnique(tag), internalRef)

/**
 * TODO: For now, `Owner` can be any capability, and it may open ways to exploits.
 * Should restrict it to a specific type only.
 * TODO: By removing the covariant what happens? Does it solve the early avoidance problem?
*/
class Box[T, @caps.use Owner^](
  private [imem] val tag: InternalRef[T]#Tag,
  private [imem] val internalRef: InternalRef[T]
) extends scinear.Linear

private[imem] object Box:
  private [imem] def unapply[T, Owner^](
    box: Box[T, Owner]^
  ): Option[(InternalRef[T]#Tag, InternalRef[T])] =
    Some((box.tag, box.internalRef))
end Box

// TODO: Separate the parameters that the compiler can infer from the ones that it cannot.
def borrowImmutBox[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}, WC^, MC^](
  self: Box[T, Owner]^
)(
  using ctx: Context[WC, MC]^{ctxOwner}
  // TODO: Make sure not capturing `self` does not break anything.
): (ImmutRef[T, newOwner], ValueHolder[newOwnerKey, Box[T, Owner]^{self}]) =
  val (tag, ref) = Box.unapply(self).get
  (borrowImmutInternal(tag, ref), ValueHolder(newBoxWithInternals(tag, ref)))

def borrowMutBox[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}, @caps.use WC^, MC^](
  self: Box[T, Owner]^
)(
  using ctx: Context[WC, MC]^{ctxOwner}
  // TODO: Make sure not capturing `self` does not break anything.
): (MutRef[T, newOwner], ValueHolder[newOwnerKey, Box[T, Owner]^{self}]) =
  val (tag, ref) = Box.unapply(self).get
  (borrowMutInternal(tag, ref), ValueHolder(newBoxWithInternals(tag, ref)))

// TODO: I guess it's possible to remove all the annotations.
@throws(classOf[IllegalStateException])
def setBox[@scinear.HideLinearity T, Owner^, ctxOwner^, @caps.use WC^, MC^](self: Box[T, Owner]^, resource: T)(using ctx: Context[WC, MC]^{ctxOwner}): Box[T, Owner]^{self} =
  // TODO: Somehow provide some interfaces, that will call the actual memory allocation and deallocation functions.
  val (tag, ref) = Box.unapply(self).get
  ref.useCheck(tag)

  ref.dropAllBorrows()
  ref.setResource(resource)
  newBoxWithInternals(tag, ref)

@throws(classOf[IllegalStateException])
def swapBox[@scinear.HideLinearity T, @caps.use Owner^, @caps.use OtherOwner^, ctxOwner^, @caps.use WC^, MC^](
  self: Box[T, Owner]^, other: Box[T, OtherOwner]^
)(using ctx: Context[WC, MC]^{ctxOwner}): (Box[T, Owner]^{self}, Box[T, OtherOwner]^{other}) =
  val (selfTag, selfRef) = Box.unapply(self).get
  val (otherTag, otherRef) = Box.unapply(other).get
  InternalRef.swap(selfTag, selfRef, otherTag, otherRef)
  (newBoxWithInternals(selfTag, selfRef), newBoxWithInternals(otherTag, otherRef))

@throws(classOf[IllegalStateException])
def derefForMoving[@scinear.HideLinearity T, Owner^, ctxOwner^, @scinear.HideLinearity S, WC^, @caps.use MC^](
  self: Box[T, Owner]^,
  moveAction: Context[WC, MC]^{ctxOwner} ?->{WC, MC} T^ ->{Owner, ctxOwner, WC, MC} S
)(using ctx: Context[WC, MC]^{ctxOwner}): S =
  val (tag, ref) = Box.unapply(self).get

  ref.useCheck(tag)

  moveAction(ref.unsafeGet())

@throws(classOf[IllegalStateException])
def moveBox[@scinear.HideLinearity T, Owner^, NewOwner^, WC^, @caps.use MC^](
  self: Box[T, Owner]^
)(using ctx: Context[WC, MC]^): Box[T, NewOwner] =
  val (tag, ref) = Box.unapply(self).get
  newBoxWithInternals(tag, ref)

def readBox[@scinear.HideLinearity T, @caps.use Owner^, S, ctxOwner^, WC^, MC^](
  box: Box[T, Owner]^,
  readAction: Context[WC, MC]^ ?-> T^ -> S
)(
  using ctx: Context[WC, MC]^{ctxOwner}
): (Box[T, Owner]^{box}, S) =
  val lf = Lifetime[{ctx, Owner}]()
  val (ref, holder) = borrowImmutBox[T, Owner, {ctx}, lf.Key, lf.Owners, WC, MC](box)(using ctx)
  val res = read[T, lf.Owners, S, ctxOwner, WC, MC](ref, readAction)(using ctx)
  val newBox = unlockHolder(lf.getKey(), holder)
  (newBox, res)

// TODO: Should redefine modification
def writeBox[@scinear.HideLinearity T, @caps.use Owner^, S, ctxOwner^, @caps.use WC^, MC^](
  box: Box[T, Owner]^, writeAction: Context[WC, MC]^ ?-> T^ -> S
)(
  using ctx: Context[WC, MC]^{ctxOwner}
): (Box[T, Owner]^{box}, S) =
  val lf = Lifetime[{ctx, Owner}]()
  val (ref, holder) = borrowMutBox[T, Owner, {ctx}, lf.Key, lf.Owners, WC, MC](box)(using ctx)
  val res = write[T, lf.Owners, S, ctxOwner, WC, MC](ref, writeAction)(using ctx)
  val newBox = unlockHolder(lf.getKey(), holder)
  (newBox, res)

def newBoxFromBackground[@scinear.HideLinearity T, WC^, MC^](value: T)(using ctx: Context[WC, MC]^): Box[T, {ctx}] =
  val (newRef, newTag) = InternalRef.newWithTag(value)
  newBoxWithInternals(newTag, newRef)

/**
  * TODO: Should make it private, then every time one wants to create a new `Box` has to somehow pass it a implicit
  * owner.
  *
  * TODO: Make sure that the `Owner` captures the `context` as well.
  */
def newBox[@scinear.HideLinearity T, Owner^](resource: T): Box[T, Owner] =
  val (newRef, newTag) = InternalRef.newWithTag(resource)
  newBoxWithInternals(newTag, newRef)

private [imem] def newBoxWithInternals[@scinear.HideLinearity T, Owner^](
  tag: InternalRef[T]#Tag,
  internalRef: InternalRef[T]
): Box[T, Owner] =
  Box(tag, internalRef)
