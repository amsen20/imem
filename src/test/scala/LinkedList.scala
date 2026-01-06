import language.experimental.captureChecking

class LinkedList[T, O1^](
  _head: imem.Box[Link[T, O1], O1] = imem.newBox[Link[T, O1], O1](None)
) extends scinear.Linear:
  val head: imem.Box[Link[T, O1], O1]^{this} = _head
end LinkedList

def newLinkedListFromBackground[T, WC^, MC^](using ctx: imem.Context[WC, MC]^): LinkedList[T, {ctx}] =
    new LinkedList[T, {ctx}]()

def newLinkedListExplicit[T, O1^]: LinkedList[T, O1] =
    new LinkedList[T, O1]()

type Link[T, O1^] = Option[imem.Box[Node[T, O1], O1]]

class Node[T, O1^](_elem: imem.Box[T, O1], _next: imem.Box[Link[T, O1], O1]) extends scinear.Linear:
  val elem: imem.Box[T, O1]^{this} = _elem
  val next: imem.Box[Link[T, O1], O1]^{this} = _next
end Node

// --------------------Implementation of List[T]--------------------

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  */
def isEmptyList[T, @caps.use O1^, @caps.use O2^, @caps.use O3^, WC^, MC^](
  self: imem.ImmutRef[LinkedList[T, O1], O2]
)(
  // TODO: Check if `^{O3}` can be moved to the `Context` type parameters.
  using ctx: imem.Context[WC, MC]^{O3}
): Boolean =
  imem.read[LinkedList[T, O1], O2, Boolean, O3, WC, MC](self,
      list =>
        val lf = imem.Lifetime[{O3, O2, O1}]()
        val (headRef, listHolder) = imem.borrowImmutBox()[lf.Key, lf.Owners](list.head)
        val res = imem.read[Link[T, O1], lf.Owners, Boolean, {O3, O2}, WC, MC](headRef, head => head.isEmpty)
        imem.unlockHolder(lf.getKey(), listHolder)
        res
  )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  */
def push[T, @caps.use O1^, @caps.use O2^ >: {O1}, @caps.use O3^, @caps.use WC^, MC^](
  self: imem.MutRef[LinkedList[T, O1], O2]^,
  elem: T
)(
  using imem.Context[WC, MC]^{O3}
): Unit =
  val (isListEmpty, self2) =
    val lf = imem.Lifetime[{O3, O2, O1}]()
    val (listRef, selfHolder) = imem.borrowImmut[LinkedList[T, O1], O2, {O3, O2}, lf.Key, lf.Owners, {WC}, {MC}](self)
    val res = isEmptyList(listRef)
    (res, imem.unlockHolder(lf.getKey(), selfHolder))

  val newNode = Node(imem.newBox[T, O1](elem), imem.newBox[Link[T, O1], O1](None))
  if isListEmpty then
    imem.writeWithLinearArg[LinkedList[T, O1], {O2}, Unit, {O3}, newNode.type, {WC}, {MC}](
      self2,
      newNode,
      ctx ?=> (list, newNode) =>
        imem.setBox[Link[T, O1], {O1}, {ctx}, {WC}, {MC}](list.head, Some(imem.newBox(newNode)))
    )
  else
    imem.writeWithLinearArg[LinkedList[T, O1], {O2}, Unit, {O3}, newNode.type, {WC}, {MC}](
      self2,
      newNode,
      (list, newNode) =>
        val tempHead = imem.newBox[Link[T, O1], O1](Some(imem.newBox[Node[T, O1], O1](newNode)))
        val (tempHead2, listHead) = imem.swapBox(tempHead, list.head)
        val listHead2 =
          val lf = imem.Lifetime[{O3, O2, O1}]()
          val (listHeadRef, listHeadHolder) = imem.borrowMutBox()[lf.Key, lf.Owners](listHead)
          imem.writeWithLinearArg[Link[T, O1], lf.Owners, Unit, {O3, O2}, tempHead2.type, {WC}, {MC}](
            listHeadRef,
            tempHead2,
            ctx ?=> (head, tempHead2) =>
              val nodeBox = head.get
              val nodeBox2 =
                val lfInner = imem.Lifetime[{ctx, O1}]()
                val (nodeRef, nodeBoxHolder) = imem.borrowMutBox()[lfInner.Key, lfInner.Owners](nodeBox)
                imem.writeWithLinearArg[Node[T, O1], lfInner.Owners, Unit, {ctx}, tempHead2.type, {WC}, {MC}](
                  nodeRef,
                  tempHead2,
                  ctx ?=> (node, tempHead2) =>
                    imem.swapBox[Link[T, O1], {O1}, {O1}, {ctx}, {WC}, {MC}](node.next, tempHead2)
                )
                imem.unlockHolder(lfInner.getKey(), nodeBoxHolder) // FIXME: Just consuming it
              nodeBox2
            )

          imem.unlockHolder(lf.getKey(), listHeadHolder) // FIXME: Just consuming it
        listHead2
    )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  * @param O4 The returned box owner (which contains the popped element)
  */
def pop[T, @caps.use O1^, @caps.use O2^ >: {O1}, @caps.use O3^, @caps.use O4^ >: {O1, O2}, @caps.use WC^, @caps.use MC^](
  self: imem.MutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context[WC, MC]^{O3}
): /* TODO: Think about the moving box */Option[imem.Box[T, O4]] =
  val (isListEmpty, self2) =
    val lf = imem.Lifetime[{O3, O2, O1}]()
    val (listRef, selfHolder) = imem.borrowImmut[LinkedList[T, O1], O2, {O3, O2}, lf.Key, lf.Owners, {WC}, {MC}](self)
    val res = isEmptyList(listRef)
    (res, imem.unlockHolder(lf.getKey(), selfHolder))

  if isListEmpty then
    self2
    None
  else
    val (currentHead, self3) =
      val lf = imem.Lifetime[{O3, O4}]()
      val (listRef, selfHolder) = imem.borrowMut[LinkedList[T, O1], O2, {O3, O2}, lf.Key, lf.Owners, {WC}, {MC}](self2)
      val currentHead = imem.newBox[Link[T, O1], O4](None)
      val res = imem.writeWithLinearArg[LinkedList[T, O1], lf.Owners, imem.Box[Link[T, O1], O4], {O3}, currentHead.type, {WC}, {MC}](
        listRef,
        currentHead,
        ctx ?=> (list, currentHead) =>
          val (newCurrentHead, listHead) = imem.swapBox[Link[T, O1], {O4}, {O1}, {ctx}, {WC}, {MC}](currentHead, list.head)
          listHead // FIXME: just to consume it
          newCurrentHead
      )
      (res, imem.unlockHolder(lf.getKey(), selfHolder))

    // NOTE: A lot of things can go (and might) go wrong here.
    // It's good to make a `ShouldNotWork` test out of each of them.
    val currentHead2 =
      val lf = imem.Lifetime[{O3, O4}]()
      val (currentHeadRef, currentHeadHolder) = imem.borrowMutBox()[lf.Key, lf.Owners](currentHead)
      imem.writeWithLinearArg[Link[T, O1], lf.Owners, Unit, {O3}, self3.type, {WC}, {MC}](
        currentHeadRef,
        self3,
        ctx ?=> (head, self3) =>
          if head.isEmpty then
            self3
            None
          else
            val nodeBox = head.get
            val nodeBox2 =
              // TODO: The type parameter should be inferred from the context.
              val lfInner = imem.Lifetime[{ctx, O1}]()
              // ?: What will happen if `nodeRef` is leaked?
              val (nodeRef, nodeBoxHolder) = imem.borrowMutBox()[lfInner.Key, lfInner.Owners](nodeBox)
              val res = imem.writeWithLinearArg[LinkedList[T, O1], {O2}, Unit, {ctx}, nodeRef.type, {WC}, {MC}](
                self3,
                nodeRef,
                ctx ?=> (list, nodeRef) =>
                  imem.writeWithLinearArg[Node[T, O1], lfInner.Owners, Unit, {ctx}, list.type, {WC}, {MC}](
                    nodeRef,
                    list,
                    ctx ?=> (node, list) => imem.swapBox[Link[T, O1], {O1}, {O1}, {ctx}, {WC}, {MC}](node.next, list.head)
                  )
              )
              imem.unlockHolder(lfInner.getKey(), nodeBoxHolder) // FIXME: Just consuming it
            nodeBox2
      )
      imem.unlockHolder(lf.getKey(), currentHeadHolder)

    imem.derefForMoving[Link[T, O1], O4, {O3}, Option[imem.Box[T, O4]], {WC}, {MC}](
      currentHead2,
      head =>
        if head.isEmpty then
          None
        else
          val nodeBox = head.get
          val movedNodeBox = imem.moveBox[Node[T, O1], O1, O4, {WC}, {MC}](nodeBox)
          val res = imem.derefForMoving[Node[T, O1], O4, {O3}, imem.Box[T, O4], {WC}, {MC}](
            movedNodeBox,
            node => imem.moveBox[T, O1, {O4}, {WC}, {MC}](node.elem)
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
def peek[T, @caps.use O1^, @caps.use O2^, O3^, O4Key, @caps.use O4^ >: {O1, O2, O3}, WC^, MC^](
  self: imem.ImmutRef[LinkedList[T, O1], O2]
)(
  using imem.Context[WC, MC]^{O3}
): Option[imem.ImmutRef[T, O4]] =
  imem.read[LinkedList[T, O1], O2, Option[imem.ImmutRef[T, O4]], {O3, O2}, {WC}, {MC}](self,
    list =>
      // FIXME: val (headRef, listHeadHolder) = imem.borrowImmutBox[Link[T, O1], O1, {O3, O2}, O4Key, O4, {WC}, {MC}](list.head)
      val (headRef, listHeadHolder) = imem.borrowImmutBox()[O4Key, O4](list.head)
      listHeadHolder // FIXME: Just to consume it
      imem.read[Link[T, O1], O4, Option[imem.ImmutRef[T, O4]], {O3, O2}, {WC}, {MC}](headRef,
        head => head.map(nodeBox =>
          val (nodeRef, nodeBoxHolder) = imem.borrowImmutBox()[O4Key, O4](nodeBox)
          nodeBoxHolder // FIXME: Just to consume it
          imem.read[Node[T, O1], O4, imem.ImmutRef[T, O4], {O4}, {WC}, {MC}](
            nodeRef,
            node =>
              val (res, nodeElemHolder) = imem.borrowImmutBox()[O4Key, O4](node.elem)
              nodeElemHolder // FIXME: Just to consume it
              res
          )
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
def peekMut[T, @caps.use O1^, O2^, O3^, O4Key, @caps.use O4^ >: {O1, O2, O3}, @caps.use WC^, MC^](
  self: imem.MutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context[WC, MC]^{O3}
): Option[imem.MutRef[T, O4]] =
  imem.write[LinkedList[T, O1], O2, Option[imem.MutRef[T, O4]], {O3, O2}, {WC}, {MC}](self,
    list =>
      val (headRef, listHeadHolder) = imem.borrowMutBox()[O4Key, O4](list.head)
      listHeadHolder // FIXME: Just to consume it
      imem.write[Link[T, O1], O4, Option[imem.MutRef[T, O4]], {O3, O2}, {WC}, {MC}](headRef,
        (ctx: imem.Context[{WC}, {MC}]^{O4}) ?=> head =>
          if head.isEmpty then
            None
          else
            val nodeBox = head.get
            val (nodeRef, nodeBoxHolder) = imem.borrowMutBox()[O4Key, O4](nodeBox)
            nodeBoxHolder // FIXME: Just to consume it
            val res = imem.write[Node[T, O1], O4, imem.MutRef[T, O4], {O4}, {WC}, {MC}](
              nodeRef,
              (ctx: imem.Context[{WC}, {MC}]^{O4}) ?=> node => imem.borrowMutBox()[O4Key, O4](node.elem)._1
            )
            Some(res)
      )
  )
