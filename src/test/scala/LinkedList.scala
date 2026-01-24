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

    // return the result and unlocks the `selfHolder` to get the `self` reference
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
  // understand whether the list is empty:
  val (isListEmpty, self2) =
    // new lifetime for borrowing `self` immutably
    val lf = Lifetime[{ctx, O2}]()

    // re-borrow `self` immutably
    val (listRef, selfHolder) = borrowImmut[List[T, O1], O2, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](self)
    // checks if the list is empty
    val isListEmpty = isEmptyList(listRef)

    // return the result and unlock the `selfHolder` to get the `self` reference
    (isListEmpty, unlockHolder(lf.getKey(), selfHolder))

  if isListEmpty then
    // converge if branches by consuming `self2`
    self2.consume()
    // return None, no element to pop
    None
  else
    // temporary box to hold the list's head
    val (tempHead, self3) =
      // new lifetime for borrowing `self2` mutably
      val lf = Lifetime[{ctx, O3}]()

      // borrow `self2` mutably
      val (listRef, selfHolder) = borrowMut[List[T, O1], O2, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](self2)

      // create the temporary box
      // the temporary box's lifetime capture set is `{O3}`, as it is going to be returned
      val tempHead = newBox[Link[T, O1], O3](None)
      // the state at here: {(temp head -> None), (list -> first node), (first node -> second node and the rest of the list)}

      // access the `listRef`'s resource, which is the list, mutably
      val tempHead2 = writeWithLinearArg[List[T, O1], lf.Owners, Box[Link[T, O1], O3], {ctx}, tempHead.type, {WC}, {MC}](
        listRef,
        tempHead,
        ctx ?=> (list, tempHead) =>
          // swap the `tempHead` and the `list.head`, so that the `tempHead` points to the first node
          val (newTempHead, listHead) = swapBox[Link[T, O1], {O3}, {O1}, {ctx}, {WC}, {MC}](tempHead, list.head)
          // the state at here: {(temp head -> first node), (list -> None), (first node -> second node and the rest of the list)}

          // `listHead` is a linear variable and have to be used
          // use `listHead` by consuming
          listHead.consume()
          // return the newly created temporary box pointing to the first node
          newTempHead
      )

      // return the temporary box and unlock the `selfHolder` to get the `self2` reference
      (tempHead2, unlockHolder(lf.getKey(), selfHolder))

    val tempHead2 =
      // new lifetime for borrowing `tempHead` mutably
      val lf = Lifetime[{ctx, O3}]()

      // borrow `tempHead` mutably
      val (tempHeadRef, tempHeadHolder) = borrowMutBox[Link[T, O1], O3, {ctx, O2}, lf.Key, lf.Owners, {WC}, {MC}](tempHead)

      // access the `tempHeadRef`'s resource, which is the link to the list's first node, mutably
      writeWithLinearArg[Link[T, O1], lf.Owners, Unit, {ctx}, self3.type, {WC}, {MC}](
        tempHeadRef,
        self3,
        ctx ?=> (head, self3) =>
          // new lifetime for borrowing the list's first node mutably
          val lfInner = Lifetime[{ctx, O1}]()

          // borrow the list's first node mutably
          val (firstNodeRef, firstNodeBoxHolder) = borrowMutBox[Node[T, O1], O1, {ctx}, lfInner.Key, lfInner.Owners, {WC}, {MC}](head.get)

          // access the `self3`'s resource, which is the list, mutably
          writeWithLinearArg[List[T, O1], {O2}, Unit, {ctx}, firstNodeRef.type, {WC}, {MC}](
            self3,
            firstNodeRef,
            ctx ?=> (list, firstNodeRef) =>

              // access the `firstNodeRef`'s resource, which is the first node, mutably
              writeWithLinearArg[Node[T, O1], lfInner.Owners, Unit, {ctx}, list.type, {WC}, {MC}](
                firstNodeRef,
                list,
                ctx ?=> (firstNode, list) =>
                  // swap the node's next and the list's head
                  swapBox[Link[T, O1], {O1}, {O1}, {ctx}, {WC}, {MC}](firstNode.next, list.head)
                  // the state at here: {(temp head -> first node), (list -> second node and the rest of the list), (first node -> None)}
                  ()
              )
          )

          // unlock the `firstNodeBoxHolder`, to use both `lfInner` and `firstNodeBoxHolder`
          unlockHolder(lfInner.getKey(), firstNodeBoxHolder)
      )

      // return the temporary box and unlock the `tempHeadHolder` to get the `tempHead` reference
      unlockHolder(lf.getKey(), tempHeadHolder)

    // in here, the `tempHead2` points to the popped first node and the list points to the rest of the list
    // the runtime memory state is correct but the lifetime capture set of the `tempHead2` still contains `{O1}`
    // `tempHead2`'s type is: `Box[Option[Box[Node[T, O1], O1]], O3]`

    // access the `tempHead2`'s resource, which is the link to the popped node
    derefForMoving[Link[T, O1], O3, {ctx}, Option[Box[T, O3]], {WC}, {MC}](
      tempHead2,
      link =>
        // box to the popped node
        val nodeBox = link.get
        // access the `nodeBox`'s resource, which is the popped node
        val movedElem = derefForMoving[Node[T, O1], O1, {ctx}, Box[T, O3], {WC}, {MC}](
          nodeBox,
          // move the box to popped node's element to the new owner
          node => moveBox[T, O1, {O3}, {WC}, {MC}](node.elem)
        )

        // return the moved element box
        Some(movedElem)
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
  using ctx: Context[WC, MC]^{O3}
): Option[ImmutRef[T, O4]] =
  // access the `self`'s reference resource, which is the list
  read[List[T, O1], O2, Option[ImmutRef[T, O4]], {ctx}, {WC}, {MC}](self,
    ctx ?=> list =>
      // borrow the list's head immutably
      val (headRef, listHeadHolder) = borrowImmutBox[Link[T, O1], O1, {ctx}, O4Key, O4, {WC}, {MC}](list.head)

      // use `listHeadHolder` by consuming because it is a linear variable that is no longer needed
      listHeadHolder.consume()

      // access the head's (`headRef`'s) resource, which is a link to the next node
      read[Link[T, O1], O4, Option[ImmutRef[T, O4]], {ctx}, {WC}, {MC}](headRef,
        ctx ?=> linkOpt =>
          // without consuming the option, check whether `linkOpt` is empty or not
          val (linkOpt2, isListEmpty) = scinear.utils.peekLinearOption(linkOpt)

          // the behavior changes based on list emptiness
          if isListEmpty then
            // converge if branches by consuming `linkOpt2`
            linkOpt2.isEmpty
            // return None, no element to peek
            None
          else
            // get the box pointing to the list's first node
            val nodeBox = linkOpt2.get
            // borrow the first node immutably
            val (nodeRef, nodeBoxHolder) = borrowImmutBox[Node[T, O1], O1, {ctx}, O4Key, O4, {WC}, {MC}](nodeBox)
            // use `nodeBoxHolder` by consuming because it is a linear variable that is no longer needed
            nodeBoxHolder.consume()

            // access the `nodeRef`'s resource, which is the first node
            read[Node[T, O1], O4, Option[ImmutRef[T, O4]], {ctx}, {WC}, {MC}](
              nodeRef,
              ctx ?=> node =>
                // borrow the first node's element immutably
                val (nodeRef, nodeElemHolder) = borrowImmutBox[T, O1, {ctx}, O4Key, O4, {WC}, {MC}](node.elem)
                // use `nodeElemHolder` by consuming because it is a linear variable that is no longer needed
                // the function only needs the reference to the element
                nodeElemHolder.consume()
                // return the reference to list's first node's element
                Some(nodeRef)
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
  // access the `self`'s reference resource, which is the list
  write[List[T, O1], O2, Option[MutRef[T, O4]], {ctx}, {WC}, {MC}](self,
    ctx ?=> list =>
      // borrow the list's head mutably
      val (headRef, listHeadHolder) = borrowMutBox[Link[T, O1], O1, {ctx}, O4Key, O4, {WC}, {MC}](list.head)

      // use `listHeadHolder` by consuming because it is a linear variable that is no longer needed
      listHeadHolder.consume()

      // access the head's (`headRef`'s) resource, which is a link to the next node
      write[Link[T, O1], O4, Option[MutRef[T, O4]], {ctx}, {WC}, {MC}](headRef,
        ctx ?=> linkOpt =>
          // without consuming the option, check whether `linkOpt` is empty or not
          val (linkOpt2, isListEmpty) = scinear.utils.peekLinearOption(linkOpt)

          // the behavior changes based on list emptiness
          if isListEmpty then
            // converge if branches by consuming `linkOpt2`
            linkOpt2.isEmpty
            // return None, no element to peek
            None
          else
            // get the box pointing to the list's first node
            val nodeBox = linkOpt2.get
            // borrow the first node mutably
            val (nodeRef, nodeBoxHolder) = borrowMutBox[Node[T, O1], O1, {ctx}, O4Key, O4, {WC}, {MC}](nodeBox)
            // use `nodeBoxHolder` by consuming because it is a linear variable that is no longer needed
            nodeBoxHolder.consume()

            // access the `nodeRef`'s resource, which is the first node
            write[Node[T, O1], O4, Option[MutRef[T, O4]], {ctx}, {WC}, {MC}](
              nodeRef,
              ctx ?=> node =>
                // borrow the first node's element mutably
                val (nodeRef, nodeElemHolder) = borrowMutBox[T, O1, {ctx}, O4Key, O4, {WC}, {MC}](node.elem)
                // use `nodeElemHolder` by consuming because it is a linear variable that is no longer needed
                // the function only needs the reference to the element
                nodeElemHolder.consume()
                // return the mutable reference to list's first node's element
                Some(nodeRef)
            )
      )
  )


// --------------------Definition of ConsumingIterator[T, O]--------------------

class ConsumingIterator[T <: scinear.Linear, O^](_list: Box[List[T, O], O]) extends scinear.Linear:
	val list: Box[List[T, O], O]^{this} = _list
end ConsumingIterator

def hasNextCI[T <: scinear.Linear, @caps.use O1^, @caps.use O2^ >: {O1}, WC^, MC^](
  self: ImmutRef[ConsumingIterator[T, O1], O2]^
)(
  using ctx: Context[WC, MC]^
): Boolean =
  // access the `self`'s reference resource, which is the consuming iterator
  read[ConsumingIterator[T, O1], O2, Boolean, {ctx}, {WC}, {MC}](self,
    ctx ?=> iter =>
      // new lifetime for borrowing the iterator's list box immutably
      val lf = Lifetime[{ctx, O2}]()

      // borrow the iterator's list box immutably
      val (listRef, listHolder) = borrowImmutBox[List[T, O1], O1, {ctx}, lf.Key, lf.Owners, {WC}, {MC}](iter.list)
      // check whether the list is empty
      val hasNext = !isEmptyList[T, {O1}, lf.Owners, {WC}, {MC}](listRef)

      // use both `lf` and `listHolder` linear variables
      unlockHolder(lf.getKey(), listHolder).consume()

      // return the result
      hasNext
  )

def nextCI[T <: scinear.Linear, @caps.use O1^, @caps.use O2^ >: {O1}, @caps.use WC^, @caps.use MC^](
  self: MutRef[ConsumingIterator[T, O1], O2]^
)(
  using ctx: Context[WC, MC]^
): Option[Box[T, O2]] =
  // access the `self`'s reference resource, which is the consuming iterator
  val listRef = write[ConsumingIterator[T, O1], O2, MutRef[List[T, O1], {O2, ctx}], {ctx}, {WC}, {MC}](self,
    ctxInner ?=> iter =>
      // borrow the iterator's list box mutably
      val (listRef, listHolder) = borrowMutBox[List[T, O1], O1, {ctxInner}, NeverUsableKey, {O2, ctx}, {WC}, {MC}](iter.list)
      // use `listHolder` by consuming because it is a linear variable that is no longer needed
      listHolder.consume()
      // return the mutable reference to the iterator's list
      listRef
  )

  // pop an element from the list
  val poppedElem = pop[T, {O1}, {O2, ctx}, {O2, ctx}, {WC}, {MC}](listRef)
  // here the popped element's runtime value matches the function's return value
  // but, its lifetime capture set is {O2}, and the function returns a box with
  // with lifetime capture set {O2}

  // move the `poppedElem` from {O2, ctx} to {O2}
  val movedPoppedElem =
    // without consuming the option, check whether `poppedElem` is empty or not
    val (poppedElem2, isNone) = scinear.utils.peekLinearOption(poppedElem)
    if isNone then
      // converge if branches by consuming `poppedElem2`
      poppedElem2.get
      // return None with the correct type
      None
    else
      // move the box pointing to the popped element to the needed owner
      Some(moveBox[T, {O2, ctx}, {O2}, {WC}, {MC}](poppedElem2.get))

  // return the moved popped element
  movedPoppedElem

// -------------------Implementation of ConsumingIterator[T, O]-------------------

def moveAllElems[T <: scinear.Linear, @caps.use O1^, O2^, WC^, @caps.use MC^](
  self: Box[Link[T, O1], O1]^
)(
  using ctx: Context[WC, MC]^
): Box[Link[T, O2], O2] =
  // dereference the `self` box, to access the `self`'s resource, which is a link to the next node
  derefForMoving[Link[T, O1], O1, {ctx}, Box[Link[T, O2], O2], {WC}, {MC}](
    self,
    ctx ?=> nextOpt =>
      // without consuming the option, check whether `nextOpt` is empty or not
      val (nextOpt2, isEmpty) = scinear.utils.peekLinearOption(nextOpt)

      // the behavior changes based on whether there is a next node or not
      if isEmpty then
        // converge if branches by consuming `nextOpt2`
        nextOpt2.get
        // return a box pointing to None
        newBox[Link[T, O2], O2](None)
      else
        // get the box pointing to the next node
        val nodeBox = nextOpt2.get
        // access the `nodeBox`'s resource, which is the next node
        val newBoxToLink = derefForMoving[Node[T, O1], O1, {ctx}, Box[Link[T, O2], O2], {WC}, {MC}](
          nodeBox,
          node =>
            // get the element and the next link of the node
            val (nodeElem, nodeNext) = Node.unapply(node)
            // move the element
            val movedElemBox = moveBox[T, {O1}, O2, {WC}, {MC}](nodeElem)
            // move all elements from the next link recursively
            val movedNextBox = moveAllElems[T, {O1}, O2, {WC}, {MC}](nodeNext)
            // create a new node in owner lifetime set {O2} with the moved element and moved next link
            val newNode = Node[T, O2](movedElemBox, movedNextBox)
            // return a box pointing to a link that points to the new node
            newBox[Link[T, O2], O2](Some(newBox[Node[T, O2], O2](newNode)))
        )
        // return the new box to link that points the new next node
        newBoxToLink
  )


def intoIter[T <: scinear.Linear, @caps.use O1^, O2^, WC^, @caps.use MC^](
  self: Box[List[T, O1], O1]
)(
  using ctx: Context[WC, MC]^
): ConsumingIterator[T, O2] =
  // dereference the `self` box, to access the `self`'s resource, which is the list
  derefForMoving[List[T, O1], O1, {ctx}, ConsumingIterator[T, {O2}], {WC}, {MC}](
    self,
    list =>
      // move all elements from owner {O1} to owner {O2}
      val movedListHead = moveAllElems[T, {O1}, O2, {WC}, {MC}](list.head)
      // create a new list in owner {O2} with the moved head
      val newList = List[T, {O2}](movedListHead)
      // create a new box pointing to the new list with owner lifetime set {O2}
      val newListBox = newBox[List[T, {O2}], {O2}](newList)
      // create and return the consuming iterator
      ConsumingIterator[T, {O2}](newListBox)
  )

// --------------------Definition of MutableIterator[T, O1, O2]--------------------

class MutableIterator[T <: scinear.Linear, O1^, O2^](_boxToLink: Box[MutRef[Link[T, O1], O2], O2]) extends scinear.Linear:
  val boxToLink: Box[MutRef[Link[T, O1], O2], O2]^{this} = _boxToLink
end MutableIterator

def hasNextMI[T <: scinear.Linear, @caps.use O1^, O3^, @caps.use O2^ >: {O1, O3}, WC^, MC^](
  self: ImmutRef[MutableIterator[T, O1, O2], O3]^
)(
  using ctx: Context[WC, MC]^{O3}
): Boolean =
  // access the `self`'s reference resource, which is the mutable iterator
  read[MutableIterator[T, O1, O2], O3, Boolean, {ctx}, {WC}, {MC}](self,
    ctx ?=> iter =>
      // new lifetime for borrowing the iterator's box to the link,
      // which is pointing to the next node to be visited, immutably
      val lf = Lifetime[{O2}]()

      // borrow the iterator's box to the link, immutably
      val (refToLink, boxToLinkHolder) = borrowImmutBox[MutRef[Link[T, O1], O2], O2, {ctx}, lf.Key, lf.Owners, {WC}, {MC}](iter.boxToLink)
      // access the `refToLink`'s resource, which is the mutable reference to the link pointing to the next node to be visited
      val hasNext = read[MutRef[Link[T, O1], O2], lf.Owners, Boolean, {ctx, O3}, {WC}, {MC}](refToLink,
        ctx ?=> linkMutRef =>
          // new lifetime for borrowing the mutable reference to the link immutably
          val lf = Lifetime[{ctx, O2}]()

          // borrow the link mutable reference immutably
          val (linkRef, linkHolder) = borrowImmut[Link[T, O1 ], O2, {ctx, O3}, lf.Key, lf.Owners, {WC}, {MC}](linkMutRef)
          // access the `linkRef`'s resource, which is a link to the next node to be visited
          val hasNext = read[Link[T, O1], lf.Owners, Boolean, {ctx, O3}, {WC}, {MC}](linkRef,
            ctx ?=> link =>
              // check whether the link is empty or not
              !link.isEmpty
          )

          // use both `lf` and `linkHolder` linear variables
          unlockHolder(lf.getKey(), linkHolder).consume()

          // return the result
          hasNext
      )

      // use both `lf` and `boxToLinkHolder` linear variables
      unlockHolder(lf.getKey(), boxToLinkHolder).consume()

      // return the result
      hasNext
  )

def nextMI[T <: scinear.Linear, @caps.use O1^, O3^, @caps.use O2^ >: {O1, O3}, @caps.use WC^, MC^](
  self: MutRef[MutableIterator[T, O1, O2], O3]^
)(
  using ctx: Context[WC, MC]^{O3}
): Option[MutRef[T, O2]] =
  // access the `self`'s reference resource, which is the mutable iterator
  write[MutableIterator[T, O1, O2], {O3}, Option[MutRef[T, O2]], {O2}, {WC}, {MC}](self,
    ctx ?=> iter =>
      val boxToLink = iter.boxToLink

      // create a dummy box pointing to a mutable reference that points to a link that points to `None` for swapping with `boxToLink`:
      val dummyBoxToLink =
        // create the box pointing to a link that is `None`
        val dummyLinkBox = newBox[Link[T, O1], O2](None)
        // borrow the box pointing to a link
        val (dummyLinkMutRef, dummyBoxHolder) = borrowMutBox[Link[T, O1], O2, {ctx}, NeverUsableKey, {O2}, {WC}, {MC}](dummyLinkBox)
        // use `dummyBoxHolder` by consuming because it is a linear variable that is not needed
        dummyBoxHolder.consume()
        // create and return the box pointing to the mutable reference to the link, which is `None`
        newBox[MutRef[Link[T, O1], O2], O2](dummyLinkMutRef)

      // swap the iterators' box to link with a dummy box pointing to `None`, so that the function can access the box's value without:
      // - expiring the iterators' box
      // - being forced to define a new lifetime for borrowing that will appear in every subsequent reference borrowed
      //   and making it incompatible with the function's return type
      val (iterBoxToLink, tempBoxToLink) = swapBox[MutRef[Link[T, O1], O2], {O2}, {O2}, {ctx}, {WC}, {MC}](boxToLink, dummyBoxToLink)

      // borrow the `tempBoxToLink` mutably
      val (refToLink, tempBoxToLinkHolder) = borrowMutBox[MutRef[Link[T, O1], O2], O2, {ctx}, NeverUsableKey, {O2}, {WC}, {MC}](tempBoxToLink)
      // use `tempBoxToLinkHolder` by consuming because it is a linear variable that is no longer needed
      tempBoxToLinkHolder.consume()
      // access the `refToLink`'s resource, which is the mutable reference to the link pointing to the next node to be visited
      writeWithLinearArg[MutRef[Link[T, O1], O2], O2, Option[MutRef[T, O2]], {ctx}, Box[MutRef[Link[T, O1], O2], O2], {WC}, {MC}](refToLink,
        iterBoxToLink,
        ctx ?=> (linkMutRef, iterBoxToLink) =>
          // access the `linkMutRef`'s resource, which is a link to the next node to be visited
          writeWithLinearArg[Link[T, O1], O2, Option[MutRef[T, O2]], {ctx}, Box[MutRef[Link[T, O1], O2], O2], {WC}, {MC}](linkMutRef,
            iterBoxToLink,
            ctx ?=> (link, iterBoxToLink) =>
              // without consuming the option, check whether `link` is empty or not
              val (link2, isEmpty) = scinear.utils.peekLinearOption(link)

              // the behavior changes based on whether there is a next node or not
              if isEmpty then
                // converge if branches by consuming `link2`
                link2.get
                // leave iterator's box to link unchanged, it is now pointing to `None`, which is correct
                iterBoxToLink.consume()
                // return None, no next element
                None
              else
                // get the box pointing to the next node to be visited
                val nodeBox = link2.get
                // borrow the next node to be visited mutably
                val (nodeRef, nodeBoxHolder) = borrowMutBox[Node[T, O1], O1, {ctx}, NeverUsableKey, {O2}, {WC}, {MC}](nodeBox)
                // use `nodeBoxHolder` by consuming because it is a linear variable that is not needed
                nodeBoxHolder.consume()
                // access the `nodeRef`'s resource, which is the next node to be visited
                val elemMutRef = writeWithLinearArg[Node[T, O1], {O2}, MutRef[T, O2], {ctx}, Box[MutRef[Link[T, O1], O2], O2], {WC}, {MC}](nodeRef,
                  iterBoxToLink,
                  ctx ?=> (node, iterBoxToLink) =>
                    // decompose the node to get its element and next link
                    val (nodeElem, nodeNext) = Node.unapply(node)

                    // borrow the box pointing to the link pointing to the next node mutably
                    val (nextLinkMutRef, nextLinkBoxHolder) = borrowMutBox[Link[T, O1], O1, {ctx}, NeverUsableKey, {O2}, {WC}, {MC}](nodeNext)
                    // use `nextLinkBoxHolder` by consuming because it is a linear variable that is not needed
                    nextLinkBoxHolder.consume()

                    // update the iterator's box to link to point to the next node's link
                    setBox(iterBoxToLink, nextLinkMutRef)

                    // borrow the node's element mutably
                    val (nodeElemMutRef, nodeElemHolder) = borrowMutBox[T, O1, {ctx}, NeverUsableKey, {O2}, {WC}, {MC}](nodeElem)
                    // use `nodeElemHolder` by consuming because it is a linear variable that is not needed
                    nodeElemHolder.consume()

                    // return the mutable reference to the node's element
                    nodeElemMutRef
                )

                // return the mutable reference to the next element
                Some(elemMutRef)
            )
      )
  )

def iterMut[T <: scinear.Linear, @caps.use O1^, O3^, O2^ >: {O1, O3}, @caps.use WC^, MC^](
  self: MutRef[List[T, O1], O2]
)(
  using ctx: Context[WC, MC]^{O3}
): MutableIterator[T, O1, O2] =
  // access the `self`'s reference resource, which is the list
  write[List[T, O1], O2, MutableIterator[T, O1, O2], {ctx}, {WC}, {MC}](self,
    ctx ?=> list =>
      // borrow the list's head mutably
      val (headRef, listHeadHolder) = borrowMutBox[Link[T, O1], O1, {ctx}, NeverUsableKey, {O2}, {WC}, {MC}](list.head)
      // use `listHeadHolder` by consuming because it is a linear variable that is not needed
      listHeadHolder.consume()
      // create and return the mutable iterator
      MutableIterator[T, O1, O2](newBox[MutRef[Link[T, O1], O2], O2](headRef))
  )
