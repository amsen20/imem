
import language.experimental.captureChecking

class LinkedList[T, O1^](val head: imem.Box[Link[T, O1], O1]^{O1} = imem.newBoxExplicit[Link[T, O1], O1](None))

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
  imem.read[LinkedList[T, O1]^{O1}, O2, Boolean, {ctx}](
    self,
    (newCtx: imem.Context^{ctx, O2}) ?=> (list: LinkedList[T, O1]^{O1}) =>
      val headRef = imem.borrowImmutBox[Link[T, O1], O1, {newCtx}, {ctx, O2, O1}](list.head)
      imem.read[Link[T, O1], {ctx, O2, O1}, Boolean, {newCtx}](headRef, (head: Link[T, O1]) => head.isEmpty)
  )

def push[T, @caps.use O1^, O2^](self: imem.MutRef[LinkedList[T, O1]^{O1}, O2]^{O2}, elem: T)(using imem.Context^{O2}): Unit =
  val newNode = Node(imem.newBoxExplicit[T, O1](elem), imem.newBoxExplicit[Link[T, O1], O1](None))
  if isEmptyList(imem.borrowImmut(self)) then imem.write(self, list => imem.setBox(list.head, Some(imem.newBoxExplicit(newNode))))
  else
    imem.write(self, list =>
      imem.swapBox(newNode.next, list.head)
      imem.setBox(list.head, Some(imem.newBoxExplicit[Node[T, O1]^{O1}, O1](newNode)))
    )

def pop[T, @caps.use O1^, @caps.use O2^, O3^ >: {O1, O2}](self: imem.MutRef[LinkedList[T, O1]^{O1}, O2]^{O2})(using ctx: imem.Context^): Option[imem.Box[T, O3]^{O3}] =
  if isEmptyList(imem.borrowImmut(self)) then None
  else
    val list = imem.borrowMut(self)
    val currentHead = imem.write(self, (list: LinkedList[T, O1]^{O1}) =>
      val currentHead = imem.newBoxExplicit[Link[T, O1], O2](None)
      imem.swapBox(currentHead, list.head)
      currentHead
    )

    // NOTE: A lot of things can go (and might) go wrong here.
    // It's good to make a `ShouldNotWork` test out of each of them.
    imem.writeBox(currentHead, newCtx ?=> (head: Link[T, O1]) => head.map(nodeBox => {
      imem.write(self, newCtx ?=> list => imem.writeBox(nodeBox, (node: Node[T, O1]^{O1}) => imem.swapBox(node.next, list.head)))
      imem.writeBox(nodeBox, node => imem.moveBox(node.elem))
    }))

def peek[T, @caps.use O1^, @caps.use O2^, @caps.use O3^ >: {O1, O2}](self: imem.ImmutRef[LinkedList[T, O1]^{O1}, O2]^{O2})(using
    imem.Context^{O2}
): Option[imem.ImmutRef[T, O3]^{O3}] =
  imem.read[LinkedList[T, O1]^{O1}, O2, Option[imem.ImmutRef[T, O3]^{O3}], {O2}](self, (list: LinkedList[T, O1]^{O1}) =>
    val headRef = imem.borrowImmutBox[Link[T, O1], O1, {O2}, O3](list.head)
    imem.read[Link[T, O1], O3, Option[imem.ImmutRef[T, O3]^{O3}], {O2}](headRef, newCtx ?=> (head: Link[T, O1]) => head.map(nodeBox =>
        val nodeRef: imem.ImmutRef[Node[T, O1]^{O1}, {O3}]^{O3} = imem.borrowImmutBox[Node[T, O1]^{O1}, O1, {newCtx}, O3](nodeBox)
        imem.read[Node[T, O1]^{O1}, O3, imem.ImmutRef[T, O3]^{O3}, {newCtx}](nodeRef, newCtx ?=> (node: Node[T, O1]^{O1}) => imem.borrowImmutBox[T, O1, {newCtx}, O3](node.elem))
      )
    ))

def peekMut[T, @caps.use O1^, O2^, @caps.use O3^ >: {O1, O2}](self: imem.MutRef[LinkedList[T, O1]^{O1}, O2]^{O2})(using ctx: imem.Context^{O2}): Option[imem.MutRef[T, O3]^{O3}] =
  imem.write[LinkedList[T, O1]^{O1}, O2, Option[imem.MutRef[T, O3]^{O3}], {O2}](self, (list: LinkedList[T, O1]^{O1}) =>
    val headRef = imem.borrowMutBox[Link[T, O1], O1, {O2}, O3](list.head)
    imem.write[Link[T, O1], O3, Option[imem.MutRef[T, O3]^{O3}], {O2}](headRef, newCtx ?=> (head: Link[T, O1]) =>
      head.map(nodeBox =>
        val nodeRef: imem.MutRef[Node[T, O1]^{O1}, {O3}]^{O3} = imem.borrowMutBox[Node[T, O1]^{O1}, O1, {newCtx}, O3](nodeBox)
        imem.write[Node[T, O1]^{O1}, O3, imem.MutRef[T, O3]^{O3}, {newCtx}](nodeRef, newCtx ?=> (node: Node[T, O1]^{O1}) => imem.borrowMutBox[T, O1, {newCtx}, O3](node.elem))
      )
  ))
