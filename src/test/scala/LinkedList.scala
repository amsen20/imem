import language.experimental.captureChecking
import imem.*

// --------------------Definition of LinkedList[T, O]--------------------

class List[T <: scinear.Linear, O^](
  _head: Box[Link[T, O], O] = newBox[Link[T, O], O](None)
) extends scinear.Linear:
  val head: Box[Link[T, O], O]^{this} = _head
end List

type Link[T <: scinear.Linear, O^] = Option[Box[Node[T, O], O]]

class Node[T <: scinear.Linear, O^](_elem: Box[T, O], _next: Box[Link[T, O], O]) extends scinear.Linear:
  val elem: Box[T, O]^{this} = _elem
  val next: Box[Link[T, O], O]^{this} = _next
end Node

object Node:
  def unapply[T <: scinear.Linear, O^](node: Node[T, O]^): (Box[T, O]^{node}, Box[Link[T, O], O]^{node}) =
    (node.elem, node.next)
end Node

// --------------------Implementation of LinkedList[T, O]--------------------

def newLinkedListFromBackground[T <: scinear.Linear, WC^, MC^](using ctx: Context[WC, MC]^): List[T, {ctx}] =
		new List[T, {ctx}]()

def newLinkedListExplicit[T <: scinear.Linear, O1^]: List[T, O1] =
		new List[T, O1]()

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  */
def isEmptyList[T <: scinear.Linear, @caps.use O1^, @caps.use O2^, WC^, MC^](
  self: ImmutRef[List[T, O1], O2]
)(
  using ctx: Context[WC, MC]^
): Boolean =
  // access the `self`'s reference resource
  read[List[T, O1], O2, Boolean, {ctx}, WC, MC](self,
      list =>
        // new lifetime for borrowing the list's head
        val lf = Lifetime[{ctx, O2, O1}]()

        // borrow the list's head immutably, the new immutable reference will be `headRef`
        val (headRef, listHolder) = borrowImmutBox[Link[T, O1], O1, {ctx, O2}, lf.Key, lf.Owners, WC, MC](list.head)
        // access the head's (`headRef`'s) resource which is a `Link` to the next node
        // then check if the link whether is empty or not
        val isListEmpty = read[Link[T, O1], lf.Owners, Boolean, {ctx, O2}, WC, MC](headRef, link => link.isEmpty)

        // unlock the `listHolder`, to use both `lf` and `listHolder` linear variables
        unlockHolder(lf.getKey(), listHolder)
        // return the result
        isListEmpty
  )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  */
def push[T <: scinear.Linear, @caps.use O1^, @caps.use O2^ >: {O1}, @caps.use WC^, MC^](
  self: MutRef[List[T, O1], O2]^,
  elem: T
)(
  using ctx: Context[WC, MC]^
): Unit =
  // understand whether the list is empty:
  val (isListEmpty, self2) =
    // new lifetime for borrowing `self` immutably
    val lf = Lifetime[{ctx, O2}]()

    // re-borrow `self` immutably
    val (listRef, selfHolder) = borrowImmut[List[T, O1], O2, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](self)
    // checks if the list is empty
    val isListEmpty = isEmptyList(listRef)

    // returns the result and unlocks the `selfHolder` to get the `self` reference.
    (isListEmpty, unlockHolder(lf.getKey(), selfHolder))

  // create a the new node that is going to be pushed:
  val newNode = Node(
    newBox[T, O1](elem), // node's element
    newBox[Link[T, O1], O1](None) // node's next link, which is initially `None`
  )

  if isListEmpty then
    // access the list mutably
    writeWithLinearArg[List[T, O1], {O2}, Unit, {ctx}, newNode.type, {WC}, {MC}](
      self2, // the mutable reference to the list
      newNode,
      ctx ?=> (list, newNode) =>
        // set the list's head to point to the new node
        setBox[Link[T, O1], {O1}, {ctx}, {WC}, {MC}](list.head, Some(newBox(newNode)))
    )
  else
    // access the list mutably
    writeWithLinearArg[List[T, O1], {O2}, Unit, {ctx}, newNode.type, {WC}, {MC}](
      self2,
      newNode,
      ctx ?=> (list, newNode) =>
        // create a temporary box pointing to a link that points to the new node
        val tempHead = newBox[Link[T, O1], O1](Some(newBox[Node[T, O1], O1](newNode)))
        // the state at here: {(temp head -> new node), (list -> previous first node), (new node -> None)}

        // swap the `tempHead` and the `list.head`, so that the `list.head` points to the new node
        val (tempHead2, listHead) = swapBox(tempHead, list.head)
        // the state at here: {(temp head -> previous first node) and (list -> new node), (newNode -> None)}

        // new lifetime for borrowing the list's head mutably
        val lf = Lifetime[{ctx, O1}]()

        // borrow the list's head mutably
        val (listHeadRef, listHeadHolder) = borrowMutBox[Link[T, O1], O1, {ctx}, lf.Key, lf.Owners, {WC}, {MC}](listHead)

        // access the `listHeadRef`'s resource, which is list's head, mutably
        writeWithLinearArg[Link[T, O1], lf.Owners, Unit, {ctx, O2}, tempHead2.type, {WC}, {MC}](
          listHeadRef,
          tempHead2,
          ctx ?=> (head, tempHead2) =>
            // get the head's box, that is pointing to the newly created node
            val nodeBox = head.get

            // new lifetime for borrowing the new node mutably
            val lfInner = Lifetime[{ctx, O1}]()

            // borrow the new node mutably
            val (nodeRef, nodeBoxHolder) = borrowMutBox[Node[T, O1], O1, {ctx}, lfInner.Key, lfInner.Owners, {WC}, {MC}](nodeBox)
            // access the `nodeRef`'s resource, which is the new node, mutably
            writeWithLinearArg[Node[T, O1], lfInner.Owners, Unit, {ctx}, tempHead2.type, {WC}, {MC}](
              nodeRef,
              tempHead2,
              ctx ?=> (node, tempHead2) =>
                // swap the new node's next and the temporary box
                swapBox[Link[T, O1], {O1}, {O1}, {ctx}, {WC}, {MC}](node.next, tempHead2)
                // the state at here: {(temp head -> None), (list -> new node), (new node -> previous first node)}
                // by here, the new node is pushed to the list
                ()
            )

            // unlock the `nodeBoxHolder`, to use both `lfInner` and `nodeBoxHolder` linear variables
            unlockHolder(lfInner.getKey(), nodeBoxHolder)
            ()
          )

        // unlock the `listHeadHolder`, to use both `lf` and `listHeadHolder` linear variables
        unlockHolder(lf.getKey(), listHeadHolder)
        ()
    )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The returned box owner (which contains the popped element)
  */
def pop[T <: scinear.Linear, @caps.use O1^, @caps.use O2^ >: {O1}, @caps.use O3^ >: {O2}, @caps.use WC^, @caps.use MC^](
  self: MutRef[List[T, O1], O2]
)(
  using ctx: Context[WC, MC]^
): Option[Box[T, O3]] =
  val (isListEmpty, self2) =
    val lf = Lifetime[{ctx, O2}]()
    val (listRef, selfHolder) = borrowImmut[List[T, O1], O2, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](self)
    val res = isEmptyList(listRef)
    (res, unlockHolder(lf.getKey(), selfHolder))

  if isListEmpty then
    self2.consume()
    None
  else
    val (currentHead, self3) =
      val lf = Lifetime[{ctx, O3}]()
      val (listRef, selfHolder) = borrowMut[List[T, O1], O2, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](self2)
      val currentHead = newBox[Link[T, O1], O3](None)
      val res = writeWithLinearArg[List[T, O1], lf.Owners, Box[Link[T, O1], O3], {ctx}, currentHead.type, {WC}, {MC}](
        listRef,
        currentHead,
        ctx ?=> (list, currentHead) =>
          val (newCurrentHead, listHead) = swapBox[Link[T, O1], {O3}, {O1}, {ctx}, {WC}, {MC}](currentHead, list.head)
          listHead.consume()
          newCurrentHead
      )
      (res, unlockHolder(lf.getKey(), selfHolder))

    val currentHead2 =
      val lf = Lifetime[{ctx, O3}]()
      val (currentHeadRef, currentHeadHolder) = borrowMutBox[Link[T, O1], O3, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](currentHead)
      writeWithLinearArg[Link[T, O1], lf.Owners, Unit, {ctx}, self3.type, {WC}, {MC}](
        currentHeadRef,
        self3,
        ctx ?=> (head, self3) =>
          val (headOpt, isHeadEmpty) = scinear.utils.peekLinearOption(head)
          if isHeadEmpty then
            self3.consume()
            headOpt.isEmpty
            None
          else
            val nodeBox = headOpt.get
            val nodeBox2 =
              val lfInner = Lifetime[{ctx, O1}]()
              val (nodeRef, nodeBoxHolder) = borrowMutBox[Node[T, O1], O1, {ctx}, lfInner.Key, lfInner.Owners, {WC}, {MC}](nodeBox)
              val res = writeWithLinearArg[List[T, O1], {O2}, Unit, {ctx}, nodeRef.type, {WC}, {MC}](
                self3,
                nodeRef,
                ctx ?=> (list, nodeRef) =>
                  writeWithLinearArg[Node[T, O1], lfInner.Owners, Unit, {ctx}, list.type, {WC}, {MC}](
                    nodeRef,
                    list,
                    ctx ?=> (node, list) =>
                      swapBox[Link[T, O1], {O1}, {O1}, {ctx}, {WC}, {MC}](node.next, list.head)
                      ()
                  )
              )
              unlockHolder(lfInner.getKey(), nodeBoxHolder)
            nodeBox2.consume()
      )
      unlockHolder(lf.getKey(), currentHeadHolder)

    derefForMoving[Link[T, O1], O3, {ctx}, Option[Box[T, O3]], {WC}, {MC}](
      currentHead2,
      head =>
        if head.isEmpty then
          None
        else
          val nodeBox = head.get
          val movedNodeBox = moveBox[Node[T, O1], O1, O3, {WC}, {MC}](nodeBox)
          val res = derefForMoving[Node[T, O1], O3, {ctx}, Box[T, O3], {WC}, {MC}](
            movedNodeBox,
            node => moveBox[T, O1, {O3}, {WC}, {MC}](node.elem)
          )
          Some(res)
    )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  * @param O4 The returned reference owner (which contains the peeked element)
*/
def peek[T <: scinear.Linear, @caps.use O1^, @caps.use O2^, O3^, O4Key, @caps.use O4^ >: {O1, O2, O3}, WC^, MC^](
  self: ImmutRef[List[T, O1], O2]
)(
  using Context[WC, MC]^{O3}
): Option[ImmutRef[T, O4]] =
  read[List[T, O1], O2, Option[ImmutRef[T, O4]], {O3, O2}, {WC}, {MC}](self,
    list =>
      val (headRef, listHeadHolder) = borrowImmutBox[Link[T, O1], O1, {O3, O2}, O4Key, O4, {WC}, {MC}](list.head)
      listHeadHolder.consume()
      read[Link[T, O1], O4, Option[ImmutRef[T, O4]], {O3, O2}, {WC}, {MC}](headRef,
        head =>
          val (headOpt, isHeadEmpty) = scinear.utils.peekLinearOption(head)
          if isHeadEmpty then
            headOpt.isEmpty
            None
          else
            val nodeBox = headOpt.get
            val (nodeRef, nodeBoxHolder) = borrowImmutBox[Node[T, O1], O1, {O4}, O4Key, O4, {WC}, {MC}](nodeBox)
            nodeBoxHolder.consume()
            read[Node[T, O1], O4, Option[ImmutRef[T, O4]], {O4}, {WC}, {MC}](
              nodeRef,
              node =>
                val (res, nodeElemHolder) = borrowImmutBox[T, O1, {O4}, O4Key, O4, {WC}, {MC}](node.elem)
                nodeElemHolder.consume()
                Some(res)
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
def peekMut[T <: scinear.Linear, @caps.use O1^, O2^, O3^, O4Key, @caps.use O4^ >: {O1, O2, O3}, @caps.use WC^, MC^](
  self: MutRef[List[T, O1], O2]
)(
  using ctx: Context[WC, MC]^{O3}
): Option[MutRef[T, O4]] =
  write[List[T, O1], O2, Option[MutRef[T, O4]], {O3, O2}, {WC}, {MC}](self,
    list =>
      val (headRef, listHeadHolder) = borrowMutBox[Link[T, O1], O1, {O3, O2}, O4Key, O4, {WC}, {MC}](list.head)
      listHeadHolder.consume()
      write[Link[T, O1], O4, Option[MutRef[T, O4]], {O3, O2}, {WC}, {MC}](headRef,
        (ctx: Context[{WC}, {MC}]^{O4}) ?=> head =>
          if head.isEmpty then
            None
          else
            val nodeBox = head.get
            val (nodeRef, nodeBoxHolder) = borrowMutBox[Node[T, O1], O1, {O4}, O4Key, O4, {WC}, {MC}](nodeBox)
            nodeBoxHolder.consume()
            val res = write[Node[T, O1], O4, MutRef[T, O4], {O4}, {WC}, {MC}](
              nodeRef,
              node => borrowMutBox[T, O1, {O4}, O4Key, O4, {WC}, {MC}](node.elem)._1
            )
            Some(res)
      )
  )


// --------------------Definition of ConsumingIterator[T, O]--------------------

// class ConsumingIterator[T <: scinear.Linear, O^](_list: Box[LinkedList[T, O], O]) extends scinear.Linear:
// 	val list: Box[LinkedList[T, O], O]^{this} = _list
// end ConsumingIterator

// def hasNext[T <: scinear.Linear, @caps.use O1^, O2^, WC^, MC^](
//   self: ImmutRef[ConsumingIterator[T, O1], O1]
// )(
//   using ctx: Context[WC, MC]^
// ): Boolean =
//   self
//   ???

// // -------------------Implementation of ConsumingIterator[T, O]-------------------

// def moveAllElems[T <: scinear.Linear, @caps.use O1^, O2^ >: {O1}, WC^, @caps.use MC^](
//   self: Box[Link[T, O1], O1]^
// )(
//   using ctx: Context[WC, MC]^
// ): Box[Link[T, O2], O2] =
//   derefForMoving[Link[T, O1], O1, {ctx}, Box[Link[T, O2], O2], {WC}, {MC}](
//     self,
//     head =>
//       if head.isEmpty then
//         newBox[Link[T, O2], O2](None)
//       else
//         val nodeBox = head.get
//         val movedNodeBox = moveBox[Node[T, O1], O1, O2, {WC}, {MC}](nodeBox)
//         val res = derefForMoving[Node[T, O1], O2, {ctx}, Box[Link[T, O2], O2], {WC}, {MC}](
//           movedNodeBox,
//           node =>
//             val (nodeElem, nodeNext) = Node.unapply(node)
//             val movedElemBox = moveBox[T, {O1}, O2, {WC}, {MC}](nodeElem)
//             val movedNextBox = moveAllElems[T, {O1}, O2, {WC}, {MC}](nodeNext)
//             val newNode = Node[T, O2](movedElemBox, movedNextBox)
//             newBox[Link[T, O2], O2](Some(newBox[Node[T, O2], O2](newNode)))
//         )
//         res
//   )


// def intoIter[T <: scinear.Linear, @caps.use O1^, O2^ >: {O1}, WC^, @caps.use MC^](self: Box[LinkedList[T, O1], O1])(
//   using ctx: Context[WC, MC]^
// ): ConsumingIterator[T, O2] =
//   derefForMoving[LinkedList[T, O1], O1, {ctx}, ConsumingIterator[T, {O2}], {WC}, {MC}](
//     self,
//     list =>

//       val movedList = LinkedList[T, {O2}](movedListHead)
//       val movedListBox = newBox[LinkedList[T, {O2}], {O2}](movedList)
//       ConsumingIterator[T, {O2}](movedListBox)
//   )

// def hasNext

// def intoIter(): Iterator[T] = new Iterator[T] {
//   def hasNext: Boolean = head.isDefined
//   def next(): T = pop().getOrElse(throw new NoSuchElementException("next on empty iterator"))
// }

// def iter(): Iterator[T] = new Iterator[T] {
//   private var current: Link[T] = head
//   def hasNext: Boolean = current.isDefined
//   def next(): T = {
//     current match {
//       case Some(node) =>
//         val elem = node.elem
//         current = node.next
//         elem
//       case None =>
//         throw new NoSuchElementException("next on empty iterator")
//     }
//   }
// }

// def iterMut(): Iterator[Node[T]] = new Iterator[Node[T]] {
//   private var current: Link[T] = head
//   def hasNext: Boolean = current.isDefined
//   def next(): Node[T] = {
//     // We need to take the current value and advance the pointer.
//     // `take` on Option is perfect for this, as it moves the value out.
//     val nodeOpt = current.take()
//     nodeOpt match {
//       case Some(node) =>
//         current = node.next
//         node
//       case None =>
//         throw new NoSuchElementException("next on empty iterator")
//     }
//   }
// }