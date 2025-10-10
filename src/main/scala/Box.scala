package imem

import language.experimental.captureChecking

/** TODO: Maybe make it an enum, and implement the internals in the Box methods.
  */
trait BoxImpl[T, +Owner^]:
  def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner]
  def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner]

  def name: String
  override def toString(): String

case class Uninitialized[T, +Owner^]() extends BoxImpl[T, Owner]:
  override def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner] = throw new IllegalStateException(
    "Cannot borrow an uninitialized Box"
  )
  override def name: String = "Uninitialized"
  override def toString(): String = "Uninitialized"
end Uninitialized

case class Live[T, +Owner^](val tag: InternalRef[T]#Tag, val internalRef: InternalRef[T]) extends BoxImpl[T, Owner]:
  override def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner] =
    ImmutRef(internalRef.newSharedRef(tag), internalRef, ctx.getParents)
  override def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner] =
    MutRef(internalRef.newMut(tag), internalRef, ctx.getParents)
  override def name: String = "Live"
  override def toString(): String = s"Live(tag: ${tag}, internalRef: ${internalRef})"
end Live

case class Dropped[T, +Owner^]() extends BoxImpl[T, Owner]:
  override def borrowImmut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner] = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def borrowMut[ctxOwner^, newOwner^ >: {ctxOwner, Owner}](using ctx: Context^{ctxOwner}): MutRef[T, newOwner] = throw new IllegalStateException(
    "Cannot borrow a dropped Box"
  )
  override def name: String = "Dropped"
  override def toString(): String = "Dropped"
end Dropped

/**
 * TODO: For now, `Owner` can be any capability, and it may open ways to exploits.
 * Should restrict it to a specific type only.
 * TODO: By removing the covariant what happens? Does it solve the early avoidance problem?
*/
class Box[T, @caps.use +Owner^]():
  // TODO: Looks dirty for accessing the inner type. Should find a better way.
  var Impl: BoxImpl[T, {Owner}] = Uninitialized()
end Box

def borrowImmutBox[T, Owner^, ctxOwner^, newOwner^ >: {ctxOwner, Owner}](self: Box[T, Owner]^)(using ctx: Context^{ctxOwner}): ImmutRef[T, newOwner] =
  self.Impl.borrowImmut

def borrowMutBox[T, Owner^, ctxOwner^, newOwner^ >: {ctxOwner, Owner}](self: Box[T, Owner]^)(using ctx: Context^{ctxOwner}): MutRef[T, newOwner] =
  self.Impl.borrowMut

// TODO: All the `self` arguments should be `Box[T, Owner]^` not `Box[T, Owner]`. This is because
// Then the user can use this functions on the boxes it's reading/writing through a reference.
@throws(classOf[IllegalStateException])
def setBox[T, Owner^](self: Box[T, Owner], value: T): Unit =
  self.Impl match
    case Uninitialized[T, Owner]() =>
      val (ref, tag) = InternalRef.newWithTag(value)
      // ?: why should I mentioned [T, Owner] every time?
      // ?: Also, why is `asInstanceOf[BoxImpl[T, Owner]]` needed?
      self.Impl = Live[T, Owner](tag, ref).asInstanceOf[BoxImpl[T, Owner]]
    case Live[T, Owner](_, _) =>
      dropBox(self)
      val (ref, tag) = InternalRef.newWithTag(value)
      self.Impl = Live(tag, ref).asInstanceOf[BoxImpl[T, Owner]]
    case Dropped[T, Owner]() =>
      throw new IllegalStateException("Cannot set a value to a dropped Box")

@throws(classOf[IllegalStateException])
def dropBox[T, Owner^](self: Box[T, Owner]): Unit =
  self.Impl match
    case Uninitialized[T, Owner]() =>
      throw new IllegalStateException("Cannot drop an uninitialized Box")
    case Live[T, Owner](_, ref) =>
      ref.drop()
      self.Impl = Dropped()
    case Dropped[T, Owner]() =>
      throw new IllegalStateException("Cannot drop an already dropped Box")

@throws(classOf[IllegalStateException])
def swapBox[T, @caps.use Owner^, @caps.use OtherOwner^](self: Box[T, Owner], other: Box[T, OtherOwner]): Unit =
  (self.Impl, other.Impl) match
    case (Live[T, Owner](tag1, ref1), Live[T, OtherOwner](tag2, ref2)) =>
      self.Impl = Live(tag2, ref2).asInstanceOf[BoxImpl[T, Owner]]
      other.Impl = Live(tag1, ref1).asInstanceOf[BoxImpl[T, OtherOwner]]
    case _ =>
      throw new IllegalStateException(
        s"Cannot swap a ${self.Impl.name} with a ${other.Impl.name}"
      )

@throws(classOf[IllegalStateException])
def moveBox[T, Owner^, NewOwner^](self: Box[T, Owner]): Box[T, NewOwner] =
  self.Impl match
    case Live[T, Owner](_, ref) =>
      val newBox = Box[T, NewOwner]()
      setBox(newBox, ref.unsafeGet())
      dropBox(self)
      newBox
    case Uninitialized[T, Owner]() =>
      throw new IllegalStateException("Cannot move an uninitialized Box")
    case Dropped[T, Owner]() =>
      throw new IllegalStateException("Cannot move a dropped Box")

def readBox[T, Owner^, S, ctxOwner^](box: Box[T, Owner]^, readAction: Context^{ctxOwner, Owner} ?=> T^ => S)(using ctx: Context^{ctxOwner}): S =
  val ref = borrowImmutBox[T, Owner, {ctx}, {ctx, Owner}](box)(using ctx)
  read[T, {ctx, Owner}, S, ctxOwner](ref, readAction)(using ctx)

def writeBox[T, Owner^, S, ctxOwner^](box: Box[T, Owner]^, writeAction: Context^{ctxOwner, Owner} ?=> T^ => S)(using ctx: Context^{ctxOwner}): S =
  val ref = borrowMutBox[T, Owner, {ctx}, {ctx, Owner}](box)(using ctx)
  write[T, {ctx, Owner}, S, ctxOwner](ref, writeAction)(using ctx)

def newBoxFromBackground[T](value: T)(using ctx: Context^): Box[T, {ctx}] =
  val ret = new Box[T, {ctx}]()
  setBox(ret, value)
  ret

/**
  * TODO: Should make it private, then every time one wants to create a new `Box` has to somehow pass it a implicit
  * owner.
  */
def newBoxExplicit[T, Owner^](value: T): Box[T, Owner] =
  val ret = new Box[T, Owner]()
  setBox(ret, value)
  ret

class OwnerOrigin:
  opaque type Key = Object
  def getKey(): Key = new Object
end OwnerOrigin

class BoxHolder[KeyType, T, Owner^](val box: Box[T, {Owner}])

def useBoxHolder[KeyType, T, Owner^, S, ctxOwner^](
  holder: BoxHolder[KeyType, T, Owner],
  key: KeyType,
  k: Context^{ctxOwner} ?=> Box[T, {Owner}]^ => S
)(using ctx: Context^{ctxOwner}): S =
  k(using ctx)(holder.box)
