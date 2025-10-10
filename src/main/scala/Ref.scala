package imem

import language.experimental.captureChecking

type Ref = ImmutRef[?, ?] | MutRef[?, ?]

class ImmutRef[T, +Owner^](
  val tag: InternalRef[T]#Tag,
  val internalRef: InternalRef[T],
  val parents: List[Ref]
)

def borrowImmut[T, Owner^](self: ImmutRef[T, Owner]^)(using ctx: Context^): ImmutRef[T, {ctx, Owner}] =
  ImmutRef(self.internalRef.newSharedRef(self.tag), self.internalRef, ctx.getParents)

// TODO: Make sure that it's not possible to smuggle out objects through read.
def read[T, Owner^, S, ctxOwner^](self: ImmutRef[T, Owner]^, readAction: Context^{ctxOwner, Owner} ?=> T^ => S)(using ctx: Context^{ctxOwner}): S =
  self.parents.foreach(_ match
  case ref: ImmutRef[?, ?] => readCheck(ref)
  case ref: MutRef[?, ?] => readCheck(ref)
  )
  ctx.pushParent(self.asInstanceOf[Ref])
  try
  self.internalRef.read(self.tag, readAction(using ctx))
  finally ctx.popParent()

def readCheck[T, Owner^](self: ImmutRef[T, Owner]): Unit =
  self.internalRef.readCheck(self.tag)

def writeCheck[T, Owner^](self: ImmutRef[T, Owner]): Unit =
  self.internalRef.useCheck(self.tag)

class MutRef[T, +Owner^](
  val tag: InternalRef[T]#Tag,
  val internalRef: InternalRef[T],
  val parents: List[Ref]
)

def borrowMut[T, Owner^](self: MutRef[T, Owner]^)(using ctx: Context^): MutRef[T, {ctx, Owner}] =
  MutRef(self.internalRef.newMut(self.tag), self.internalRef, ctx.getParents)

def borrowImmut[T, Owner^](self: MutRef[T, Owner]^)(using ctx: Context^): ImmutRef[T, {ctx, Owner}] =
  ImmutRef(self.internalRef.newSharedRef(self.tag), self.internalRef, ctx.getParents)

def write[T, Owner^, S, ctxOwner^](self: MutRef[T, Owner]^, writeAction: Context^{ctxOwner, Owner} ?=> T^ => S)(using ctx: Context^{ctxOwner}): S =
  self.parents.foreach(_ match
  case ref: ImmutRef[?, ?] => writeCheck(ref)
  case ref: MutRef[?, ?] => writeCheck(ref)
  )
  ctx.pushParent(self.asInstanceOf[Ref])
  try
  self.internalRef.write(self.tag, writeAction(using ctx))
  finally ctx.popParent()

def readCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  self.internalRef.readCheck(self.tag)

def writeCheck[T, Owner^](self: MutRef[T, Owner]): Unit =
  self.internalRef.useCheck(self.tag)
