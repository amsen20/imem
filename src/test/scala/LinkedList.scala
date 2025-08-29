
import language.experimental.captureChecking

class LinkedList[T](val head: imem.Box[Link[T]] = imem.Box[Link[T]](None))

type Link[T] = Option[imem.Box[Node[T]]]

class Node[T](val elem: imem.Box[T], val next: imem.Box[Link[T]])

// Implementation of List[T]
def push[T](self: imem.MutRef[LinkedList[T]], elem: T)(using imem.Context): Unit =
  val newNode = Node(imem.Box(elem), imem.Box(None))
  if isEmpty(self.borrowImmut) then self.write(list => list.head.set(Some(imem.Box(newNode))))
  else
    self.write(list =>
      newNode.next.swap(list.head)
      list.head.set(Some(imem.Box(newNode)))
    )

def pop[T](self: imem.MutRef[LinkedList[T]])(using imem.Context): Option[imem.Box[T]] =
  if isEmpty(self.borrowImmut) then None
  else
    val list = self.borrowMut
    val currentHead = self.write(list =>
      val currentHead = imem.Box[Link[T]](None)
      list.head.swap(currentHead)
      currentHead
    )

    // NOTE: A lot of things can go (and might) go wrong here.
    // It's good to make a `ShouldNotWork` test out of each of them.
    currentHead.borrowMut.write(_ match {
      case None => None
      case Some(nodeBox) =>
        self.write(list => nodeBox.borrowMut.write(_.next.swap(list.head)))
        Some(nodeBox.borrowMut.write(_.elem.move()))
    })

def peek[T](self: imem.ImmutRef[LinkedList[T]])(using
    imem.Context
): Option[imem.ImmutRef[T]] =
  self.read(_.head.borrowImmut.read(_ match
    case None => None
    case Some(nodeBox) =>
      Some(nodeBox.borrowImmut.read(_.elem.borrowImmut))
  ))

def peekMut[T](self: imem.MutRef[LinkedList[T]])(using imem.Context): Option [imem.MutRef[T]] =
  self.read(_.head.borrowMut.read(_ match
    case None          => None
    case Some(nodeBox) =>
      // NOTE: it's wrong that it has mut borrow access to `elem` box through read method if nodeBox.
      Some(nodeBox.borrowMut.read(_.elem.borrowMut))
  ))

def isEmpty[T](self: imem.ImmutRef[LinkedList[T]])(using imem.Context): Boolean =
  self.read(_.head.borrowImmut.read(_.isEmpty))

/** One time iterator
  *
  * NOTE: The iterator gets the `resource.Context` while creation, and then use the *same* context
  * when iterating. For now the assumption is that there is only one context in the program, but in
  * overall, this my cause some bugs.
  */
def intoIter[T](self: imem.Box[LinkedList[T]])(using imem.Context): Iterator[imem.Box[T]] =
  new Iterator[imem.Box[T]] {
    // Move self to not be accessible
    val list = self.move()
    def hasNext: Boolean = !isEmpty(list.borrowImmut)
    def next(): imem.Box[T] =
      pop(list.borrowMut).getOrElse(throw new NoSuchElementException("next on empty iterator"))
  }

trait Iterator[+A]:

  def hasNext: Boolean
  def next(): A

/** Immutable iterator
  *
  * NOTE: Same `resource.Context` problem as `intoIter`.
  */
def iterImmut[T](
    self: imem.ImmutRef[LinkedList[T]]
)(using imem.Context): Iterator [imem.ImmutRef[T]] =
  new Iterator [imem.ImmutRef[T]] {
    private var current: Option [imem.ImmutRef[Node[T]]] = self.read(
      // NOTE: this could be borrowMut and nothing would have happened.
      _.head.borrowImmut.read(_.map(_.borrowImmut))
    )
    def hasNext: Boolean = current.isDefined
    def next(): imem.ImmutRef[T] =
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
def iterMut[T](
    self: imem.MutRef[LinkedList[T]]
)(using imem.Context): Iterator [imem.MutRef[T]] =
  new Iterator [imem.MutRef[T]] {
    private var current: Option [imem.MutRef[Node[T]]] = self.read(
      // TODO: Should ban borrow muting while reading.
      // The question is will be automatically banned by exclusive capabilities?
      _.head.borrowMut.read(_.map(_.borrowMut))
    )
    def hasNext: Boolean = current.isDefined
    def next(): imem.MutRef[T] =
      current match
        case Some(nodeRef) =>
          nodeRef.read(node =>
            current = node.next.borrowMut.read(_.map(_.borrowMut))
            node.elem.borrowMut
          )
        case None =>
          throw new NoSuchElementException("next on empty iterator")
  }
