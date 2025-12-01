import language.experimental.captureChecking

class LinkedList[T, O1^](
  val _head: imem.Box[Link[T, O1], O1] = imem.newBoxExplicit[Link[T, O1], O1](None)
) extends scinear.Linear:
  // TODO: Make box capability and then there is no need for this field and also capturing `this`.
  val head: imem.Box[Link[T, O1], O1]^{this} = _head
end LinkedList

def newLinkedListFromBackground[T, WriteCap^](using ctx: imem.Context[WriteCap]^): LinkedList[T, {ctx}] =
    new LinkedList[T, {ctx}]()

def newLinkedListExplicit[T, O1^]: LinkedList[T, O1] =
    new LinkedList[T, O1]()

type Link[T, O1^] = Option[imem.Box[Node[T, O1], O1]]

// TODO: Consider `_elem` instead of `val _elem`.
class Node[T, O1^](val _elem: imem.Box[T, O1], val _next: imem.Box[Link[T, O1], O1]) extends scinear.Linear:
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
def isEmptyList[T, @caps.use O1^, @caps.use O2^, @caps.use O3^, WriteCap^](
  self: imem.ImmutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context[WriteCap]^{O3}
): Boolean =
  imem.read[LinkedList[T, O1], O2, Boolean, O3, WriteCap](self,
      list =>
        val lf = imem.Lifetime[{O3, O2, O1}]()
        val (headRef, listHolder) = imem.borrowImmutBox[Link[T, O1], O1, {O3, O2}, lf.Key, lf.Owners, WriteCap](list.head)
        val res = imem.read[Link[T, O1], lf.Owners, Boolean, {O3, O2}, WriteCap](headRef, head => head.isEmpty)
        imem.accessValue(lf.getKey(), listHolder)
        res
  )

/**
  * Type parameters:
  * @param T The type of elements in the list
  * @param O1 The list nodes owner
  * @param O2 The list reference (`self`) owner
  * @param O3 The context owner
  */
def push[T, @caps.use O1^, @caps.use O2^ >: {O1}, @caps.use O3^, @caps.use WriteCap^](
  self: imem.MutRef[LinkedList[T, O1], O2]^,
  elem: T
)(
  using imem.Context[WriteCap]^{O3}
): Unit =
  val (isListEmpty, self2) =
    val lf = imem.Lifetime[{O3, O2, O1}]()
    val (listRef, selfHolder) = imem.borrowImmut[LinkedList[T, O1], O2, {O3, O2}, lf.Key, lf.Owners, {WriteCap}](self)
    val res = isEmptyList(listRef)
    (res, imem.accessValue(lf.getKey(), selfHolder))

  val newNode = Node(imem.newBoxExplicit[T, O1](elem), imem.newBoxExplicit[Link[T, O1], O1](None))
  if isListEmpty then
    imem.writeWithLinearArg[LinkedList[T, O1], {O2}, Unit, {O3}, newNode.type, {WriteCap}](
      self2,
      newNode,
      ctx ?=> (list, newNode) =>
        imem.setBox[Link[T, O1], {O1}, {ctx}, {WriteCap}](list.head, Some(imem.newBoxExplicit(newNode)))
    )
  else
    imem.writeWithLinearArg[LinkedList[T, O1], {O2}, Unit, {O3}, newNode.type, {WriteCap}](
      self2,
      newNode,
      (list, newNode) =>
        val tempHead = imem.newBoxExplicit[Link[T, O1], O1](Some(imem.newBoxExplicit[Node[T, O1], O1](newNode)))
        val (tempHead2, listHead) = imem.swapBox(tempHead, list.head)
        val listHead2 =
          val lf = imem.Lifetime[{O3, O2, O1}]()
          val (listHeadRef, listHeadHolder) = imem.borrowMutBox[Link[T, O1], O1, {O3, O2}, lf.Key, lf.Owners, {WriteCap}](listHead)
          imem.writeWithLinearArg[Link[T, O1], lf.Owners, Unit, {O3, O2}, tempHead2.type, {WriteCap}](
            listHeadRef,
            tempHead2,
            ctx ?=> (head, tempHead2) =>
              val nodeBox = head.get
              val nodeBox2 =
                val lfInner = imem.Lifetime[{ctx, O1}]()
                val (nodeRef, nodeBoxHolder) = imem.borrowMutBox[Node[T, O1], O1, {ctx}, lfInner.Key, lfInner.Owners, {WriteCap}](nodeBox)
                imem.writeWithLinearArg[Node[T, O1], lfInner.Owners, Unit, {ctx}, tempHead2.type, {WriteCap}](
                  nodeRef,
                  tempHead2,
                  ctx ?=> (node, tempHead2) =>
                    imem.swapBox[Link[T, O1], {O1}, {O1}, {ctx}, {WriteCap}](node.next, tempHead2)
                )
                imem.accessValue(lfInner.getKey(), nodeBoxHolder) // FIXME: Just consuming it
              nodeBox2
            )

          imem.accessValue(lf.getKey(), listHeadHolder) // FIXME: Just consuming it
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
def pop[T, @caps.use O1^, @caps.use O2^ >: {O1}, @caps.use O3^, @caps.use O4^ >: {O1, O2}, @caps.use WriteCap^](
  self: imem.MutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context[WriteCap]^{O3}
): /* TODO: Think about the moving box */Option[imem.Box[T, O4]] =
  val (isListEmpty, self2) =
    val lf = imem.Lifetime[{O3, O2, O1}]()
    val (listRef, selfHolder) = imem.borrowImmut[LinkedList[T, O1], O2, {O3, O2}, lf.Key, lf.Owners, {WriteCap}](self)
    val res = isEmptyList(listRef)
    (res, imem.accessValue(lf.getKey(), selfHolder))

  if isListEmpty then
    self2
    None
  else
    val (currentHead, self3) =
      val lf = imem.Lifetime[{O3, O4}]()
      val (listRef, selfHolder) = imem.borrowMut[LinkedList[T, O1], O2, {O3, O2}, lf.Key, lf.Owners, {WriteCap}](self2)
      val currentHead = imem.newBoxExplicit[Link[T, O1], O4](None)
      val res = imem.writeWithLinearArg[LinkedList[T, O1], lf.Owners, imem.Box[Link[T, O1], O4], {O3}, currentHead.type, {WriteCap}](
        listRef,
        currentHead,
        ctx ?=> (list, currentHead) =>
          val (newCurrentHead, listHead) = imem.swapBox[Link[T, O1], {O4}, {O1}, {ctx}, {WriteCap}](currentHead, list.head)
          listHead // FIXME: just to consume it
          newCurrentHead
      )
      (res, imem.accessValue(lf.getKey(), selfHolder))

    // NOTE: A lot of things can go (and might) go wrong here.
    // It's good to make a `ShouldNotWork` test out of each of them.
    val currentHead2 =
      val lf = imem.Lifetime[{O3, O4}]()
      val (currentHeadRef, currentHeadHolder) = imem.borrowMutBox[Link[T, O1], O4, {O3, O2}, lf.Key, lf.Owners, {WriteCap}](currentHead)
      imem.writeWithLinearArg[Link[T, O1], lf.Owners, Unit, {O3}, self3.type, {WriteCap}](
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
              val (nodeRef, nodeBoxHolder) = imem.borrowMutBox[Node[T, O1], O1, {ctx}, lfInner.Key, lfInner.Owners, {WriteCap}](nodeBox)
              val res = imem.writeWithLinearArg[LinkedList[T, O1], {O2}, Unit, {ctx}, nodeRef.type, {WriteCap}](
                self3,
                nodeRef,
                ctx ?=> (list, nodeRef) =>
                  imem.writeWithLinearArg[Node[T, O1], lfInner.Owners, Unit, {ctx}, list.type, {WriteCap}](
                    nodeRef,
                    list,
                    ctx ?=> (node, list) => imem.swapBox[Link[T, O1], {O1}, {O1}, {ctx}, {WriteCap}](node.next, list.head)
                  )
              )
              imem.accessValue(lfInner.getKey(), nodeBoxHolder) // FIXME: Just consuming it
            nodeBox2
      )
      imem.accessValue(lf.getKey(), currentHeadHolder)

    imem.moving[Link[T, O1], O4, {O3}, Option[imem.Box[T, O4]], {WriteCap}](
      currentHead2,
      head =>
        if head.isEmpty then
          None
        else
          val nodeBox = head.get
          val movedNodeBox = imem.moveBox[Node[T, O1], O1, O4](nodeBox)
          val res = imem.moving[Node[T, O1], O4, {O3}, imem.Box[T, O4], {WriteCap}](
            movedNodeBox,
            node => imem.moveBox[T, O1, O4](node.elem)
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
def peek[T, @caps.use O1^, @caps.use O2^, O3^, O4Key, @caps.use O4^ >: {O1, O2, O3}, WriteCap^](
  self: imem.ImmutRef[LinkedList[T, O1], O2]
)(
  using imem.Context[WriteCap]^{O3}
): Option[imem.ImmutRef[T, O4]] =
  imem.read[LinkedList[T, O1], O2, Option[imem.ImmutRef[T, O4]], {O3, O2}, {WriteCap}](self,
    list =>
      val (headRef, listHeadHolder) = imem.borrowImmutBox[Link[T, O1], O1, {O3, O2}, O4Key, O4, {WriteCap}](list.head)
      listHeadHolder // FIXME: Just to consume it
      imem.read[Link[T, O1], O4, Option[imem.ImmutRef[T, O4]], {O3, O2}, {WriteCap}](headRef,
        head => head.map(nodeBox =>
          val (nodeRef, nodeBoxHolder) = imem.borrowImmutBox[Node[T, O1], O1, {O4}, O4Key, O4, {WriteCap}](nodeBox)
          nodeBoxHolder // FIXME: Just to consume it
          imem.read[Node[T, O1], O4, imem.ImmutRef[T, O4], {O4}, {WriteCap}](
            nodeRef,
            node =>
              val (res, nodeElemHolder) = imem.borrowImmutBox[T, O1, {O4}, O4Key, O4, {WriteCap}](node.elem)
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
def peekMut[T, @caps.use O1^, O2^, O3^, O4Key, @caps.use O4^ >: {O1, O2, O3}, @caps.use WriteCap^](
  self: imem.MutRef[LinkedList[T, O1], O2]
)(
  using ctx: imem.Context[WriteCap]^{O3}
): Option[imem.MutRef[T, O4]] =
  imem.write[LinkedList[T, O1], O2, Option[imem.MutRef[T, O4]], {O3, O2}, {WriteCap}](self,
    list =>
      val (headRef, listHeadHolder) = imem.borrowMutBox[Link[T, O1], O1, {O3, O2}, O4Key, O4, {WriteCap}](list.head)
      listHeadHolder // FIXME: Just to consume it
      imem.write[Link[T, O1], O4, Option[imem.MutRef[T, O4]], {O3, O2}, {WriteCap}](headRef,
        (ctx: imem.Context[{WriteCap}]^{O4}) ?=> head =>
          if head.isEmpty then
            None
          else
            val nodeBox = head.get
            val (nodeRef, nodeBoxHolder) = imem.borrowMutBox[Node[T, O1], O1, {O4}, O4Key, O4, {WriteCap}](nodeBox)
            nodeBoxHolder // FIXME: Just to consume it
            val res = imem.write[Node[T, O1], O4, imem.MutRef[T, O4], {O4}, {WriteCap}](
              nodeRef,
              node => imem.borrowMutBox[T, O1, {O4}, O4Key, O4, {WriteCap}](node.elem)._1
            )
            Some(res)
      )
  )
