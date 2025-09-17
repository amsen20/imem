
import language.experimental.captureChecking

class LinkedList[T, O1^](val head: imem.Box[Link[T, O1], O1] = imem.Box.newExplicit[Link[T, O1], O1](None))

object LinkedList:
  def newFromBackground[T](using ctx: imem.Context^): LinkedList[T, {ctx}] =
      new LinkedList[T, {ctx}]()

  def newExplicit[T, O1^]: LinkedList[T, O1] =
      new LinkedList[T, O1]()
end LinkedList

type Link[T, O1^] = Option[imem.Box[Node[T, O1], O1]]

class Node[T, O1^](val elem: imem.Box[T, O1], val next: imem.Box[Link[T, O1], O1])

/**
  * NOTE: All the owners are now marked as the same, for just making things compile and starting.
  */

// Implementation of List[T]
def isEmptyList[T, O1^, O2^](self: imem.ImmutRef[LinkedList[T, O1], O2])(using ctx: imem.Context^): Boolean =
  self.read[Boolean, {ctx}, LinkedList[T, O1]](
    (newCtx: imem.Context^{ctx, O2}) ?=> (list: LinkedList[T, O1]) =>
      list.head.borrowImmut[{newCtx}, {ctx, O2, O1}].read[Boolean, {newCtx}, Link[T, O1]]((head: Link[T, O1]) => head.isEmpty)
  )

def push[T, O1^, O2^](self: imem.MutRef[LinkedList[T, O1], O2], elem: T)(using imem.Context^{O2}): Unit =
  val newNode = Node(imem.Box.newExplicit[T, O1](elem), imem.Box.newExplicit[Link[T, O1], O1](None))
  if isEmptyList(self.borrowImmut) then self.write(list => list.head.set(Some(imem.Box.newExplicit(newNode))))
  else
    self.write(list =>
      newNode.next.swap(list.head)
      list.head.set(Some(imem.Box.newExplicit[Node[T, O1], O1](newNode)))
    )

def pop[T, O1^, O2^, O3^ >: {O1, O2}](self: imem.MutRef[LinkedList[T, O1], O2])(using ctx: imem.Context^): Option[imem.Box[T, O3]] =
  if isEmptyList(self.borrowImmut) then None
  else
    val list = self.borrowMut
    val currentHead = self.write((list: LinkedList[T, O1]) =>
      val currentHead = imem.Box.newExplicit[Link[T, O1], O2](None)
      // ?: I don't know how this will compile:
      // currentHead.swap(list.head)
      list.head.swap(currentHead)
      currentHead
    )

    // NOTE: A lot of things can go (and might) go wrong here.
    // It's good to make a `ShouldNotWork` test out of each of them.
    currentHead.borrowMut[{ctx}, {ctx, O2}].write(newCtx ?=> head => head match {
      case None => None
      case Some(nodeBox) =>
        self.write(newCtx ?=> list => nodeBox.borrowMut[{newCtx}, {ctx, O2, O1}].write((node: Node[T, O1]) => node.next.swap(list.head)))
        Some(nodeBox.borrowMut[{newCtx}, {ctx, O2, O1}].write(_.elem.move()))
    })

def peek[T, O1^, O2^, O3^ >: {O1, O2}](self: imem.ImmutRef[LinkedList[T, O1], O2])(using
    imem.Context^{O2}
): Option[imem.ImmutRef[T, O3]] =
  self.read[Option[imem.ImmutRef[T, O3]], O2, LinkedList[T, O1]](list => list.head.borrowImmut[{O2}, O3].read[Option[imem.ImmutRef[T, O3]], {O2}, Link[T, O1]](newCtx ?=> (head: Link[T, O1]) => head match
    case None => None
    case Some(nodeBox) =>
      Some(nodeBox.borrowImmut[{newCtx}, O3].read[imem.ImmutRef[T, O3], {newCtx}, Node[T, O1]](newCtx ?=> (node: Node[T, O1]) => node.elem.borrowImmut[{newCtx}, O3]))
  ))


def peekMut[T, O1^, O2^, O3^ >: {O1, O2}](self: imem.MutRef[LinkedList[T, O1], O2])(using ctx: imem.Context^{O2}): Option[imem.MutRef[T, O3]] =
  self.read[Option[imem.MutRef[T, O3]], {O2}, LinkedList[T, O1]](newCtx ?=> list => list.head.borrowMut[{newCtx}, {O3}].read[Option[imem.MutRef[T, O3]], {O2}, Link[T, O1]](newCtx ?=> node => node match
    case None => None
      case Some(nodeBox) =>
        Some(nodeBox.borrowMut[{newCtx}, O3].read[imem.MutRef[T, O3], {newCtx}, Node[T, O1]](newCtx ?=> (node: Node[T, O1]) => node.elem.borrowMut[{newCtx}, O3]))
  ))

/** One time iterator
  *
  * NOTE: The iterator gets the `resource.Context` while creation, and then use the *same* context
  * when iterating. For now the assumption is that there is only one context in the program, but in
  * overall, this my cause some bugs.
  */
def intoIter[T, O1^](self: imem.Box[LinkedList[T, O1], O1])(using ctx: imem.Context^): Iterator[imem.Box[T, {O1, ctx}]]^{ctx} =
  new Iterator[imem.Box[T, {O1, ctx}]] {
    // Move self to not be accessible
    val list = self.move()
    def hasNext: Boolean = !isEmptyList(list.borrowImmut)
    def next(): imem.Box[T, {O1, ctx}] =
      pop(list.borrowMut).getOrElse(throw new NoSuchElementException("next on empty iterator"))
  }

trait Iterator[+A]:

  def hasNext: Boolean
  def next(): A

/** Immutable iterator
  *
  * NOTE: Same `resource.Context` problem as `intoIter`.
  */
def iterImmut[T, O1^, O2^, O3^ >: {O1, O2} <: {O1, O2}](
    self: imem.ImmutRef[LinkedList[T, O1], O3]
)(using ctx: imem.Context^{O2}): Iterator [imem.ImmutRef[T, O3]]^{O2} =
  new Iterator [imem.ImmutRef[T, O3]] {
    private var current: Option [imem.ImmutRef[Node[T, O1], O3]] = self.read(
      // NOTE: this could be borrowMut and nothing would have happened.
      (list: LinkedList[T, O1]) => list.head.borrowImmut.read(_.map(_.borrowImmut))
    )
    def hasNext: Boolean = current.isDefined
    def next(): imem.ImmutRef[T, O3] =
      current match
        case Some(nodeRef) =>
          nodeRef.read(node =>
            current = node.next.borrowImmut.read(_.map(_.borrowImmut))
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
def iterMut[T, O1^, O2^, O3^ >: {O1, O2} <: {O1, O2}](
    self: imem.MutRef[LinkedList[T, O1], O1]
)(using ctx: imem.Context^{O2}): Iterator [imem.MutRef[T, O3]]^{O2} =
  new Iterator [imem.MutRef[T, O3]] {
    private var current: Option [imem.MutRef[Node[T, O1], O3]] = self.read(
      // TODO: Should ban borrow muting while reading.
      // The question is will be automatically banned by exclusive capabilities?
      newCtx ?=> list => list.head.borrowMut[{newCtx}, O3].read(_.map(_.borrowMut))
    )
    def hasNext: Boolean = current.isDefined
    def next(): imem.MutRef[T, O3] =
      current match
        case Some(nodeRef) =>
          nodeRef.read(node =>
            current = node.next.borrowMut.read(_.map(_.borrowMut))
            node.elem.borrowMut
          )
        case None =>
          throw new NoSuchElementException("next on empty iterator")
  }
