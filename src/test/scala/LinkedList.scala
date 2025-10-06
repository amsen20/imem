
import language.experimental.captureChecking

class LinkedList[T, O1^](val head: imem.Box[Link[T, O1], O1]^{O1} = imem.Box.newExplicit[Link[T, O1], O1](None))

object LinkedList:
  def newFromBackground[T](using ctx: imem.Context^): LinkedList[T, {ctx}]^{ctx} =
      new LinkedList[T, {ctx}]()

  def newExplicit[T, O1^]: LinkedList[T, O1]^{O1} =
      new LinkedList[T, O1]()
end LinkedList

type Link[T, O1^] = Option[imem.Box[Node[T, O1]^{O1}, O1]^{O1}]^{O1}

class Node[T, O1^](val elem: imem.Box[T, O1]^{O1}, val next: imem.Box[Link[T, O1], O1]^{O1})

/**
  * NOTE: All the owners are now marked as the same, for just making things compile and starting.
  */

// Implementation of List[T]
def isEmptyList[T, @caps.use O1^, O2^](self: imem.ImmutRef[LinkedList[T, O1]^{O1}, O2]^{O2})(using ctx: imem.Context^): Boolean =
  self.read[Boolean, {ctx}, LinkedList[T, O1]^{O1}](
    (newCtx: imem.Context^{ctx, O2}) ?=> (list: LinkedList[T, O1]^{O1}) =>
      list.head.borrowImmut[{newCtx}, {ctx, O2, O1}].read[Boolean, {newCtx}, Link[T, O1]]((head: Link[T, O1]) => head.isEmpty)
  )

def push[T, @caps.use O1^, O2^](self: imem.MutRef[LinkedList[T, O1]^{O1}, O2]^{O2}, elem: T)(using imem.Context^{O2}): Unit =
  val newNode = Node(imem.Box.newExplicit[T, O1](elem), imem.Box.newExplicit[Link[T, O1], O1](None))
  if isEmptyList(self.borrowImmut) then self.write(list => list.head.set(Some(imem.Box.newExplicit(newNode))))
  else
    self.write(list =>
      newNode.next.swap(list.head)
      list.head.set(Some(imem.Box.newExplicit[Node[T, O1]^{O1}, O1](newNode)))
    )

def pop[T, @caps.use O1^, @caps.use O2^, O3^ >: {O1, O2}](self: imem.MutRef[LinkedList[T, O1]^{O1}, O2]^{O2})(using ctx: imem.Context^): Option[imem.Box[T, O3]^{O3}] =
  if isEmptyList(self.borrowImmut) then None
  else
    val list = self.borrowMut
    val currentHead = self.write((list: LinkedList[T, O1]^{O1}) =>
      val currentHead = imem.Box.newExplicit[Link[T, O1], O2](None)
      currentHead.swap(list.head)
      currentHead
    )

    // NOTE: A lot of things can go (and might) go wrong here.
    // It's good to make a `ShouldNotWork` test out of each of them.
    currentHead.borrowMut[{ctx}, {ctx, O2}].write(newCtx ?=> (head: Link[T, O1]) => head match {
      case None => None
      // ?: `Some(nodeBox)` didn't work, the compiler tries to do:
      // `x1.$asInstanceOf[Some[imem.Box[Node[T, O1]^{O1}, O1]]].value` instead of
      // `x1.$asInstanceOf[Some[imem.Box[Node[T, O1]^{O1}, O1]^{O1}]].value`.
      // TODO: Check if this is actually a bug, and report it to the Scala team.
      case Some(_) =>
        val nodeBox = head.get
        self.write(newCtx ?=> list => nodeBox.borrowMut[{newCtx}, {ctx, O2, O1}].write((node: Node[T, O1]^{O1}) => node.next.swap(list.head)))
        Some(nodeBox.borrowMut[{newCtx}, {ctx, O2, O1}].write(_.elem.move()))
    })

def peek[T, @caps.use O1^, @caps.use O2^, @caps.use O3^ >: {O1, O2}](self: imem.ImmutRef[LinkedList[T, O1]^{O1}, O2]^{O2})(using
    imem.Context^{O2}
): Option[imem.ImmutRef[T, O3]^{O3}] =
  self.read[Option[imem.ImmutRef[T, O3]^{O3}], {O2}, LinkedList[T, O1]^{O1}]((list: LinkedList[T, O1]^{O1}) =>
    list.head.borrowImmut[{O2}, O3].read[Option[imem.ImmutRef[T, O3]^{O3}], {O2}, Link[T, O1]](newCtx ?=>
      (head: Link[T, O1]) => head match
        case None => None
        case Some(_) =>
          val nodeBox = head.get
          Some(nodeBox.borrowImmut[{newCtx}, O3].read[imem.ImmutRef[T, O3]^{O3}, {newCtx}, Node[T, O1]^{O1}](newCtx ?=> (node: Node[T, O1]^{O1}) => node.elem.borrowImmut[{newCtx}, O3]))
  ))


def peekMut[T, @caps.use O1^, O2^, O3^ >: {O1, O2}](self: imem.MutRef[LinkedList[T, O1]^{O1}, O2]^{O2})(using ctx: imem.Context^{O2}): Option[imem.MutRef[T, O3]^{O3}] =
  self.read[Option[imem.MutRef[T, O3]^{O3}], {O2}, LinkedList[T, O1]^{O1}]((list: LinkedList[T, O1]^{O1}) =>
    list.head.borrowMut[{O2}, O3].read[Option[imem.MutRef[T, O3]^{O3}], {O2}, Link[T, O1]](newCtx ?=> (head: Link[T, O1]) => head match
      case None => None
      case Some(_) =>
        val nodeBox = head.get
        Some(nodeBox.borrowMut[{newCtx}, O3].read[imem.MutRef[T, O3]^{O3}, {newCtx}, Node[T, O1]^{O1}](newCtx ?=> (node: Node[T, O1]^{O1}) => node.elem.borrowMut[{newCtx}, O3]))
  ))
