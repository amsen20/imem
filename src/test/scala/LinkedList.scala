
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

trait Iterator[+A]:

  def hasNext: Boolean
  def next(): A

/** One time iterator
  *
  * NOTE: The iterator gets the `resource.Context` while creation, and then use the *same* context
  * when iterating. For now the assumption is that there is only one context in the program, but in
  * overall, this my cause some bugs.
  */
def intoIter[T, @caps.use O1^](self: imem.Box[LinkedList[T, O1]^{O1}, O1]^{O1})(using ctx: imem.Context^): Iterator[imem.Box[T, {O1, ctx}]^{O1, ctx}]^{O1, ctx} =
  new Iterator[imem.Box[T, {O1, ctx}]^{O1, ctx}] {
    // Move self to not be accessible
    val list = self.move()
    def hasNext: Boolean = !isEmptyList(list.borrowImmut)
    def next(): imem.Box[T, {O1, ctx}]^{O1, ctx} =
      pop[T, {O1}, {ctx, O1}, {O1, ctx}](list.borrowMut[{ctx}, {ctx, O1}]).getOrElse(throw new NoSuchElementException("next on empty iterator"))
  }

/** Immutable iterator
  *
  * NOTE: Same `resource.Context` problem as `intoIter`.
  */
def iterImmut[T, @caps.use O1^, O2^, @caps.use O3^ >: {O1, O2} <: {O1, O2}](
    self: imem.ImmutRef[LinkedList[T, O1]^{O1}, O3]^{O3}
)(using ctx: imem.Context^{O2}): Iterator[imem.ImmutRef[T, O3]^{O3}]^{O3} =
  new Iterator [imem.ImmutRef[T, O3]^{O3}] {
    private var current: Option[imem.ImmutRef[Node[T, O1]^{O1}, O3]^{O3}] = self.read(
      // NOTE: this could be borrowMut and nothing would have happened.
      (newCtx: imem.Context^{O3}) ?=>
          (list: LinkedList[T, O1]^{O1}) =>
            list.head.borrowImmut[{O3}, {O3}].read(
              (newCtx: imem.Context^{O3}) ?=> (head: Link[T, O1]) => head.map(_.borrowImmut[{O3}, {O3}])
            )
    )
    def hasNext: Boolean = current.isDefined
    def next(): imem.ImmutRef[T, O3]^{O3} =
      current match
        case Some(_) =>
          val nodeRef = current.get
          nodeRef.read((newCtx: imem.Context^{O3}) ?=> (node: Node[T, O1]^{O1}) =>
            current = node.next.borrowImmut.read(
              (newCtx: imem.Context^{O3}) ?=> (next: Link[T, O1]) => next.map(_.borrowImmut)
            )
            // NOTE: this could be borrowMut and nothing would have happened.
            node.elem.borrowImmut
          )
        case None =>
          throw new NoSuchElementException("next on empty iterator")
  }

/** Mutable iterator
  *
  * NOTE: Same `resource.Context` problem as `intoIter`.
  */
def iterMut[T, @caps.use O1^, O2^, @caps.use O3^ >: {O1, O2} <: {O1, O2}](
    self: imem.MutRef[LinkedList[T, O1]^{O1}, O1]^{O1}
)(using ctx: imem.Context^{O2}): Iterator[imem.MutRef[T, O3]^{O3}]^{O3} =
  new Iterator[imem.MutRef[T, O3]^{O3}] {
    private var current: Option[imem.MutRef[Node[T, O1]^{O1}, O3]^{O3}] = self.read(
      // TODO: Should ban borrow muting while reading.
      // The question is will be automatically banned by exclusive capabilities?
      (newCtx: imem.Context^{O3}) ?=>
                (list: LinkedList[T, O1]^{O1}) =>
                  list.head.borrowMut[{newCtx}, O3].read(
                    (newCtx: imem.Context^{O3}) ?=> (head: Link[T, O1]) => head.map(_.borrowMut)
                  )
    )
    def hasNext: Boolean = current.isDefined
    def next(): imem.MutRef[T, O3]^{O3} =
      current match
        case Some(_) =>
          val nodeRef = current.get
          nodeRef.read((node: Node[T, O1]^{O1}) =>
            current = node.next.borrowMut.read(
              (newCtx: imem.Context^{O3}) ?=> (next: Link[T, O1]) => next.map(_.borrowMut)
            )
            node.elem.borrowMut
          )
        case None =>
          throw new NoSuchElementException("next on empty iterator")
  }
