package imem

import language.experimental.captureChecking

def borrowImmutInternal[T, Owner^, ctxOwner^, newOwner^ >: {ctxOwner, Owner}, WriteCap^](
  tag: InternalRef[T]#Tag,
  internalRef: InternalRef[T]
)(using ctx: Context[WriteCap]^{ctxOwner}): ImmutRef[T, newOwner] =
  ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)

def borrowMutInternal[T, Owner^, ctxOwner^, newOwner^ >: {ctxOwner, Owner}, WriteCap^](
  tag: InternalRef[T]#Tag,
  internalRef: InternalRef[T]
)(using ctx: Context[WriteCap]^{ctxOwner}): MutRef[T, newOwner] =
  MutRef(internalRef.newMut(tag), internalRef, ctx.getParents)

/**
 * TODO: For now, `Owner` can be any capability, and it may open ways to exploits.
 * Should restrict it to a specific type only.
 * TODO: By removing the covariant what happens? Does it solve the early avoidance problem?
*/
class Box[T, @caps.use +Owner^](
  val tag: InternalRef[T]#Tag,
  val internalRef: InternalRef[T]
) extends scinear.Linear

object Box:
  def unapply[T, Owner^](
    box: Box[T, Owner]^
  ): Option[(InternalRef[T]#Tag, InternalRef[T])] =
    Some((box.tag, box.internalRef))
end Box

// TODO: Separate the parameters that the compiler can infer from the ones that it cannot.
def borrowImmutBox[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}, WriteCap^](
  self: Box[T, Owner]^
)(
  using ctx: Context[WriteCap]^{ctxOwner}
  // TODO: Make sure not capturing `self` does not break anything.
): (ImmutRef[T, newOwner], ValueHolder[newOwnerKey, Box[T, Owner]^{self}]) =
  val (tag, ref) = Box.unapply(self).get
  (borrowImmutInternal(tag, ref), ValueHolder(newBoxWithInternals(tag, ref)))

def borrowMutBox[@scinear.HideLinearity T, Owner^, ctxOwner^, newOwnerKey, newOwner^ >: {ctxOwner, Owner}, @caps.use WriteCap^](
  self: Box[T, Owner]^
)(
  using ctx: Context[WriteCap]^{ctxOwner}
  // TODO: Make sure not capturing `self` does not break anything.
): (MutRef[T, newOwner], ValueHolder[newOwnerKey, Box[T, Owner]^{self}]) =
  val (tag, ref) = Box.unapply(self).get
  (borrowMutInternal(tag, ref), ValueHolder(newBoxWithInternals(tag, ref)))

@throws(classOf[IllegalStateException])
def setBox[@scinear.HideLinearity T, Owner^, ctxOwner^, @caps.use WriteCap^](self: Box[T, Owner]^, value: T)(using Context[WriteCap]^{ctxOwner}): Box[T, Owner]^{self} =
  // TODO: Somehow provide some interfaces, that will call the actual memory allocation and deallocation functions.
  val (tag, ref) = Box.unapply(self).get
  ref.dropAllBorrows()
  ref.setValue(value)
  newBoxWithInternals(tag, ref)

@throws(classOf[IllegalStateException])
def swapBox[@scinear.HideLinearity T, @caps.use Owner^, @caps.use OtherOwner^, ctxOwner^, @caps.use WriteCap^](
  self: Box[T, Owner]^, other: Box[T, OtherOwner]^
)(using Context[WriteCap]^{ctxOwner}): (Box[T, Owner]^{self}, Box[T, OtherOwner]^{other}) =
  val (selfTag, selfRef) = Box.unapply(self).get
  val (otherTag, otherRef) = Box.unapply(other).get
  InternalRef.swap(selfTag, selfRef, otherTag, otherRef)
  (newBoxWithInternals(selfTag, selfRef), newBoxWithInternals(otherTag, otherRef))

@throws(classOf[IllegalStateException])
def moving[@scinear.HideLinearity T, Owner^, ctxOwner^, @scinear.HideLinearity S, @caps.use WriteCap^](
  self: Box[T, Owner]^,
  movingAction: MovingContext[{Owner}] ?->{Owner, ctxOwner, WriteCap} T^ ->{Owner, ctxOwner, WriteCap} S
)(using Context[WriteCap]^{ctxOwner}): S =
  val movingContext = new MovingContext[{Owner}]
  val ref = Box.unapply(self).get._2
  movingAction(using movingContext)(ref.unsafeGet())

@throws(classOf[IllegalStateException])
def moveBox[@scinear.HideLinearity T, Owner^, NewOwner^](
  self: Box[T, Owner]^
)(
  using movingContext: MovingContext[{NewOwner}]
): Box[T, NewOwner] =
  val (tag, ref) = Box.unapply(self).get
  newBoxWithInternals(tag, ref)

def readBox[@scinear.HideLinearity T, @caps.use Owner^, S, ctxOwner^, WriteCap^](
  box: Box[T, Owner]^,
  readAction: Context[WriteCap]^ ?-> T^ -> S
)(
  using ctx: Context[WriteCap]^{ctxOwner}
): (Box[T, Owner]^{box}, S) =
  val lf = Lifetime[{ctx, Owner}]()
  val (ref, holder) = borrowImmutBox[T, Owner, {ctx}, lf.Key, lf.Owners, WriteCap](box)(using ctx)
  val res = read[T, lf.Owners, S, ctxOwner, WriteCap](ref, readAction)(using ctx)
  val newBox = accessValue(lf.getKey(), holder)
  (newBox, res)

// TODO: Should redefine modification
def writeBox[@scinear.HideLinearity T, @caps.use Owner^, S, ctxOwner^, @caps.use WriteCap^](
  box: Box[T, Owner]^, writeAction: Context[WriteCap]^ ?-> T^ -> S
)(
  using ctx: Context[WriteCap]^{ctxOwner}
): (Box[T, Owner]^{box}, S) =
  val lf = Lifetime[{ctx, Owner}]()
  val (ref, holder) = borrowMutBox[T, Owner, {ctx}, lf.Key, lf.Owners, WriteCap](box)(using ctx)
  val res = write[T, lf.Owners, S, ctxOwner, WriteCap](ref, writeAction)(using ctx)
  val newBox = accessValue(lf.getKey(), holder)
  (newBox, res)

def newBoxFromBackground[@scinear.HideLinearity T, WriteCap^](value: T)(using ctx: Context[WriteCap]^): Box[T, {ctx}] =
  val (newRef, newTag) = InternalRef.newWithTag(value)
  newBoxWithInternals(newTag, newRef)

/**
  * TODO: Should make it private, then every time one wants to create a new `Box` has to somehow pass it a implicit
  * owner.
  *
  * TODO: Make sure that the `Owner` captures the `context` as well.
  */
def newBoxExplicit[@scinear.HideLinearity T, Owner^](value: T): Box[T, Owner] =
  val (newRef, newTag) = InternalRef.newWithTag(value)
  newBoxWithInternals(newTag, newRef)

def newBoxWithInternals[@scinear.HideLinearity T, Owner^](
  tag: InternalRef[T]#Tag,
  internalRef: InternalRef[T]
): Box[T, Owner] =
  Box(tag, internalRef)
