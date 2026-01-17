import language.experimental.captureChecking

// --------------------Definition of LinkedList[T, O]--------------------

class LinkedList[T <: scinear.Linear, O^](
  _head: imem.Box[Link[T, O], O] = imem.newBox[Link[T, O], O](None)
) extends scinear.Linear:
  val head: imem.Box[Link[T, O], O]^{this} = _head
end LinkedList

type Link[T <: scinear.Linear, O^] = Option[imem.Box[Node[T, O], O]]

class Node[T <: scinear.Linear, O^](_elem: imem.Box[T, O], _next: imem.Box[Link[T, O], O]) extends scinear.Linear:
  val elem: imem.Box[T, O]^{this} = _elem
  val next: imem.Box[Link[T, O], O]^{this} = _next
end Node

object Node:
  def unapply[T <: scinear.Linear, O^](node: Node[T, O]^): (imem.Box[T, O]^{node}, imem.Box[Link[T, O], O]^{node}) =
    (node.elem, node.next)
end Node

// --------------------Implementation of LinkedList[T, O]--------------------

def newLinkedListFromBackground[T <: scinear.Linear, WC^, MC^](using ctx: imem.Context[WC, MC]^): LinkedList[T, {ctx}] =
		new LinkedList[T, {ctx}]()

def newLinkedListExplicit[T <: scinear.Linear, O1^]: LinkedList[T, O1] =
		new LinkedList[T, O1]()

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  */
def isEmptyList[T <: scinear.Linear, @caps.use O1^, @caps.use O2^, WC^, MC^](
  self: imem.ImmutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context[WC, MC]^
): Boolean =
  imem.read[LinkedList[T, O1], O2, Boolean, {ctx}, WC, MC](self,
      list =>
        val lf = imem.Lifetime[{ctx, O2, O1}]()
        val (headRef, listHolder) = imem.borrowImmutBox[Link[T, O1], O1, {ctx, O2}, lf.Key, lf.Owners, WC, MC](list.head)
        val res = imem.read[Link[T, O1], lf.Owners, Boolean, {ctx, O2}, WC, MC](headRef, head => head.isEmpty)
        imem.unlockHolder(lf.getKey(), listHolder)
        res
  )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  */
def push[T <: scinear.Linear, @caps.use O1^, @caps.use O2^ >: {O1}, @caps.use WC^, MC^](
  self: imem.MutRef[LinkedList[T, O1], O2]^,
  elem: T
)(
  using ctx: imem.Context[WC, MC]^
): Unit =
  // understand whether the list is empty:
  val (isListEmpty, self2) =
    val lf = imem.Lifetime[{ctx, O2}]()
    val (listRef, selfHolder) = imem.borrowImmut[LinkedList[T, O1], O2, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](self) // re-borrow `self` immutably
    val res = isEmptyList(listRef)
    (res, imem.unlockHolder(lf.getKey(), selfHolder)) // result and the original `self` mutable reference

  // create a the new node that is going to be pushed:
  val newNode = Node(
    imem.newBox[T, O1](elem), // node element
    imem.newBox[Link[T, O1], O1](None) // node next link, initially `None`
  )

  if isListEmpty then
    imem.writeWithLinearArg[LinkedList[T, O1], {O2}, Unit, {ctx}, newNode.type, {WC}, {MC}](
      self2,
      newNode,
      ctx ?=> (list, newNode) =>
        imem.setBox[Link[T, O1], {O1}, {ctx}, {WC}, {MC}](list.head, Some(imem.newBox(newNode)))
    )
  else
    imem.writeWithLinearArg[LinkedList[T, O1], {O2}, Unit, {ctx}, newNode.type, {WC}, {MC}](
      self2,
      newNode,
      (list, newNode) =>
        val tempHead = imem.newBox[Link[T, O1], O1](Some(imem.newBox[Node[T, O1], O1](newNode)))
        val (tempHead2, listHead) = imem.swapBox(tempHead, list.head)
        val listHead2 =
          val lf = imem.Lifetime[{ctx, O2}]()
          val (listHeadRef, listHeadHolder) = imem.borrowMutBox[Link[T, O1], O1, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](listHead)
          imem.writeWithLinearArg[Link[T, O1], lf.Owners, Unit, {ctx, O2}, tempHead2.type, {WC}, {MC}](
            listHeadRef,
            tempHead2,
            ctx ?=> (head, tempHead2) =>
              val nodeBox = head.get
              val nodeBox2 =
                val lfInner = imem.Lifetime[{ctx, O1}]()
                val (nodeRef, nodeBoxHolder) = imem.borrowMutBox[Node[T, O1], O1, {ctx}, lfInner.Key, lfInner.Owners, {WC}, {MC}](nodeBox)
                imem.writeWithLinearArg[Node[T, O1], lfInner.Owners, Unit, {ctx}, tempHead2.type, {WC}, {MC}](
                  nodeRef,
                  tempHead2,
                  ctx ?=> (node, tempHead2) =>
                    imem.swapBox[Link[T, O1], {O1}, {O1}, {ctx}, {WC}, {MC}](node.next, tempHead2)
                    ()
                )
                imem.unlockHolder(lfInner.getKey(), nodeBoxHolder)
              nodeBox2.consume()
            )

          imem.unlockHolder(lf.getKey(), listHeadHolder)
        listHead2.consume()
    )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The returned box owner (which contains the popped element)
  */
def pop[T <: scinear.Linear, @caps.use O1^, @caps.use O2^ >: {O1}, @caps.use O3^ >: {O2}, @caps.use WC^, @caps.use MC^](
  self: imem.MutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context[WC, MC]^
): Option[imem.Box[T, O3]] =
  val (isListEmpty, self2) =
    val lf = imem.Lifetime[{ctx, O2}]()
    val (listRef, selfHolder) = imem.borrowImmut[LinkedList[T, O1], O2, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](self)
    val res = isEmptyList(listRef)
    (res, imem.unlockHolder(lf.getKey(), selfHolder))

  if isListEmpty then
    self2.consume()
    None
  else
    val (currentHead, self3) =
      val lf = imem.Lifetime[{ctx, O3}]()
      val (listRef, selfHolder) = imem.borrowMut[LinkedList[T, O1], O2, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](self2)
      val currentHead = imem.newBox[Link[T, O1], O3](None)
      val res = imem.writeWithLinearArg[LinkedList[T, O1], lf.Owners, imem.Box[Link[T, O1], O3], {ctx}, currentHead.type, {WC}, {MC}](
        listRef,
        currentHead,
        ctx ?=> (list, currentHead) =>
          val (newCurrentHead, listHead) = imem.swapBox[Link[T, O1], {O3}, {O1}, {ctx}, {WC}, {MC}](currentHead, list.head)
          listHead.consume()
          newCurrentHead
      )
      (res, imem.unlockHolder(lf.getKey(), selfHolder))

    val currentHead2 =
      val lf = imem.Lifetime[{ctx, O3}]()
      val (currentHeadRef, currentHeadHolder) = imem.borrowMutBox[Link[T, O1], O3, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](currentHead)
      imem.writeWithLinearArg[Link[T, O1], lf.Owners, Unit, {ctx}, self3.type, {WC}, {MC}](
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
              val lfInner = imem.Lifetime[{ctx, O1}]()
              val (nodeRef, nodeBoxHolder) = imem.borrowMutBox[Node[T, O1], O1, {ctx}, lfInner.Key, lfInner.Owners, {WC}, {MC}](nodeBox)
              val res = imem.writeWithLinearArg[LinkedList[T, O1], {O2}, Unit, {ctx}, nodeRef.type, {WC}, {MC}](
                self3,
                nodeRef,
                ctx ?=> (list, nodeRef) =>
                  imem.writeWithLinearArg[Node[T, O1], lfInner.Owners, Unit, {ctx}, list.type, {WC}, {MC}](
                    nodeRef,
                    list,
                    ctx ?=> (node, list) =>
                      imem.swapBox[Link[T, O1], {O1}, {O1}, {ctx}, {WC}, {MC}](node.next, list.head)
                      ()
                  )
              )
              imem.unlockHolder(lfInner.getKey(), nodeBoxHolder)
            nodeBox2.consume()
      )
      imem.unlockHolder(lf.getKey(), currentHeadHolder)

    imem.derefForMoving[Link[T, O1], O3, {ctx}, Option[imem.Box[T, O3]], {WC}, {MC}](
      currentHead2,
      head =>
        if head.isEmpty then
          None
        else
          val nodeBox = head.get
          val movedNodeBox = imem.moveBox[Node[T, O1], O1, O3, {WC}, {MC}](nodeBox)
          val res = imem.derefForMoving[Node[T, O1], O3, {ctx}, imem.Box[T, O3], {WC}, {MC}](
            movedNodeBox,
            node => imem.moveBox[T, O1, {O3}, {WC}, {MC}](node.elem)
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
  self: imem.ImmutRef[LinkedList[T, O1], O2]
)(
  using imem.Context[WC, MC]^{O3}
): Option[imem.ImmutRef[T, O4]] =
  imem.read[LinkedList[T, O1], O2, Option[imem.ImmutRef[T, O4]], {O3, O2}, {WC}, {MC}](self,
    list =>
      val (headRef, listHeadHolder) = imem.borrowImmutBox[Link[T, O1], O1, {O3, O2}, O4Key, O4, {WC}, {MC}](list.head)
      listHeadHolder.consume()
      imem.read[Link[T, O1], O4, Option[imem.ImmutRef[T, O4]], {O3, O2}, {WC}, {MC}](headRef,
        head =>
          val (headOpt, isHeadEmpty) = scinear.utils.peekLinearOption(head)
          if isHeadEmpty then
            headOpt.isEmpty
            None
          else
            val nodeBox = headOpt.get
            val (nodeRef, nodeBoxHolder) = imem.borrowImmutBox[Node[T, O1], O1, {O4}, O4Key, O4, {WC}, {MC}](nodeBox)
            nodeBoxHolder.consume()
            imem.read[Node[T, O1], O4, Option[imem.ImmutRef[T, O4]], {O4}, {WC}, {MC}](
              nodeRef,
              node =>
                val (res, nodeElemHolder) = imem.borrowImmutBox[T, O1, {O4}, O4Key, O4, {WC}, {MC}](node.elem)
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
  self: imem.MutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context[WC, MC]^{O3}
): Option[imem.MutRef[T, O4]] =
  imem.write[LinkedList[T, O1], O2, Option[imem.MutRef[T, O4]], {O3, O2}, {WC}, {MC}](self,
    list =>
      val (headRef, listHeadHolder) = imem.borrowMutBox[Link[T, O1], O1, {O3, O2}, O4Key, O4, {WC}, {MC}](list.head)
      listHeadHolder.consume()
      imem.write[Link[T, O1], O4, Option[imem.MutRef[T, O4]], {O3, O2}, {WC}, {MC}](headRef,
        (ctx: imem.Context[{WC}, {MC}]^{O4}) ?=> head =>
          if head.isEmpty then
            None
          else
            val nodeBox = head.get
            val (nodeRef, nodeBoxHolder) = imem.borrowMutBox[Node[T, O1], O1, {O4}, O4Key, O4, {WC}, {MC}](nodeBox)
            nodeBoxHolder.consume()
            val res = imem.write[Node[T, O1], O4, imem.MutRef[T, O4], {O4}, {WC}, {MC}](
              nodeRef,
              node => imem.borrowMutBox[T, O1, {O4}, O4Key, O4, {WC}, {MC}](node.elem)._1
            )
            Some(res)
      )
  )


// --------------------Definition of ConsumingIterator[T, O]--------------------

// class ConsumingIterator[T <: scinear.Linear, O^](_list: imem.Box[LinkedList[T, O], O]) extends scinear.Linear:
// 	val list: imem.Box[LinkedList[T, O], O]^{this} = _list
// end ConsumingIterator

// def hasNext[T <: scinear.Linear, @caps.use O1^, O2^, WC^, MC^](
//   self: imem.ImmutRef[ConsumingIterator[T, O1], O1]
// )(
//   using ctx: imem.Context[WC, MC]^
// ): Boolean =
//   self
//   ???

// // -------------------Implementation of ConsumingIterator[T, O]-------------------

// def moveAllElems[T <: scinear.Linear, @caps.use O1^, O2^ >: {O1}, WC^, @caps.use MC^](
//   self: imem.Box[Link[T, O1], O1]^
// )(
//   using ctx: imem.Context[WC, MC]^
// ): imem.Box[Link[T, O2], O2] =
//   imem.derefForMoving[Link[T, O1], O1, {ctx}, imem.Box[Link[T, O2], O2], {WC}, {MC}](
//     self,
//     head =>
//       if head.isEmpty then
//         imem.newBox[Link[T, O2], O2](None)
//       else
//         val nodeBox = head.get
//         val movedNodeBox = imem.moveBox[Node[T, O1], O1, O2, {WC}, {MC}](nodeBox)
//         val res = imem.derefForMoving[Node[T, O1], O2, {ctx}, imem.Box[Link[T, O2], O2], {WC}, {MC}](
//           movedNodeBox,
//           node =>
//             val (nodeElem, nodeNext) = Node.unapply(node)
//             val movedElemBox = imem.moveBox[T, {O1}, O2, {WC}, {MC}](nodeElem)
//             val movedNextBox = moveAllElems[T, {O1}, O2, {WC}, {MC}](nodeNext)
//             val newNode = Node[T, O2](movedElemBox, movedNextBox)
//             imem.newBox[Link[T, O2], O2](Some(imem.newBox[Node[T, O2], O2](newNode)))
//         )
//         res
//   )


// def intoIter[T <: scinear.Linear, @caps.use O1^, O2^ >: {O1}, WC^, @caps.use MC^](self: imem.Box[LinkedList[T, O1], O1])(
//   using ctx: imem.Context[WC, MC]^
// ): ConsumingIterator[T, O2] =
//   imem.derefForMoving[LinkedList[T, O1], O1, {ctx}, ConsumingIterator[T, {O2}], {WC}, {MC}](
//     self,
//     list =>

//       val movedList = LinkedList[T, {O2}](movedListHead)
//       val movedListBox = imem.newBox[LinkedList[T, {O2}], {O2}](movedList)
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