import language.experimental.captureChecking
import imem.ImmutRef

class LinkedList[T, O1^](
  val head: imem.Box[Link[T, O1], O1] = imem.newBoxExplicit[Link[T, O1], O1](None)
)

def newLinkedListFromBackground[T](using ctx: imem.Context^): LinkedList[T, {ctx}] =
    new LinkedList[T, {ctx}]()

def newLinkedListExplicit[T, O1^]: LinkedList[T, O1] =
    new LinkedList[T, O1]()

type Link[T, O1^] = Option[imem.Box[Node[T, O1], O1]]

class Node[T, O1^](val elem: imem.Box[T, O1], val next: imem.Box[Link[T, O1], O1])

// --------------------Implementation of List[T]--------------------

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  */
def isEmptyList[T, @caps.use O1^, O2^, O3^](
  self: imem.ImmutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context^{O3}
): Boolean =
  imem.read[LinkedList[T, O1], O2, Boolean, O3](self,
      list =>
        val headRef = imem.borrowImmutBox[Link[T, O1], O1, {O3, O2}, {O3, O2, O1}](list.head)
        imem.read[Link[T, O1], {O3, O2, O1}, Boolean, {O3, O2}](headRef, head => head.isEmpty)
  )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  */
def push[T, @caps.use O1^, O2^, O3^](
  self: imem.MutRef[LinkedList[T, O1], O2],
  elem: T
)(
  using imem.Context^{O3}
): Unit =
  val newNode = Node(imem.newBoxExplicit[T, O1](elem), imem.newBoxExplicit[Link[T, O1], O1](None))
  if isEmptyList(imem.borrowImmut(self)) then
    imem.write[LinkedList[T, O1], O2, Unit, O3](self, list => imem.setBox(list.head, Some(imem.newBoxExplicit(newNode))))
  else
    imem.write[LinkedList[T, O1], O2, Unit, O3](self, list =>
      imem.swapBox(newNode.next, list.head)
      imem.setBox(list.head, Some(imem.newBoxExplicit[Node[T, O1], O1](newNode)))
    )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  * @param O4 The returned box owner (which contains the popped element)
  */
def pop[T, @caps.use O1^, @caps.use O2^, O3^, O4^ >: {O1, O2}](
  self: imem.MutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context^{O3}
): Option[imem.Box[T, O4]] =
  if isEmptyList(imem.borrowImmut(self)) then None
  else
    val list = imem.borrowMut(self)
    val currentHead = imem.write[LinkedList[T, O1], O2, imem.Box[Link[T, O1], O2], O3](self,
      list =>
        val currentHead = imem.newBoxExplicit[Link[T, O1], O2](None)
        imem.swapBox(currentHead, list.head)
        currentHead
    )

    // NOTE: A lot of things can go (and might) go wrong here.
    // It's good to make a `ShouldNotWork` test out of each of them.
    imem.writeBox[Link[T, O1], O2, Option[imem.Box[T, O4]], O3](currentHead,
      head => head.map(nodeBox => {
        imem.write[LinkedList[T, O1], O2, Unit, {O3, O2}](self,
            list => imem.writeBox[Node[T, O1], O1, Unit, {O3, O2}](nodeBox, node => imem.swapBox(node.next, list.head))
        )
        imem.writeBox[Node[T, O1], O1, imem.Box[T, O4], {O3, O2}](nodeBox, node => imem.moveBox(node.elem))
      })
    )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  * @param O4 The returned reference owner (which contains the peeked element)
*/
def peek[T, @caps.use O1^, @caps.use O2^, O3^, @caps.use O4^ >: {O1, O2, O3}](
  self: imem.ImmutRef[LinkedList[T, O1], O2]
)(
  using imem.Context^{O3}
): Option[imem.ImmutRef[T, O4]] =
  imem.read[LinkedList[T, O1], O2, Option[imem.ImmutRef[T, O4]], {O3, O2}](self,
    list =>
      val headRef = imem.borrowImmutBox[Link[T, O1], O1, {O3, O2}, O4](list.head)
      imem.read[Link[T, O1], O4, Option[imem.ImmutRef[T, O4]], {O3, O2}](headRef,
        head => head.map(nodeBox =>
          val nodeRef = imem.borrowImmutBox[Node[T, O1], O1, {O4}, O4](nodeBox)
          imem.read[Node[T, O1], O4, imem.ImmutRef[T, O4], {O4}](nodeRef, node => imem.borrowImmutBox[T, O1, {O4}, O4](node.elem))
        )
      )
  )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  * @param O4 The returned reference owner (which contains the peeked element)
*/
def peekMut[T, @caps.use O1^, O2^, O3^, @caps.use O4^ >: {O1, O2, O3}](
  self: imem.MutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context^{O3}
): Option[imem.MutRef[T, O4]] =
  imem.write[LinkedList[T, O1], O2, Option[imem.MutRef[T, O4]], {O3, O2}](self,
    list =>
      val headRef = imem.borrowMutBox[Link[T, O1], O1, {O3, O2}, O4](list.head)
      imem.write[Link[T, O1], O4, Option[imem.MutRef[T, O4]], {O3, O2}](headRef,
        head => head.map(nodeBox =>
            val nodeRef = imem.borrowMutBox[Node[T, O1], O1, {O4}, O4](nodeBox)
            imem.write[Node[T, O1], O4, imem.MutRef[T, O4], {O4}](nodeRef, node => imem.borrowMutBox[T, O1, {O4}, O4](node.elem))
          )
      )
  )
