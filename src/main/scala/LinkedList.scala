package imem

import language.experimental.captureChecking

class List[T](val head: resource.Box[Link[T]] = resource.Box[Link[T]](None))

type Link[T] = Option[resource.Box[Node[T]]]

class Node[T](val elem: resource.Box[T], val next: resource.Box[Link[T]])

// Implementation of List[T]
def push[T](self: resource.MutRef[List[T]], elem: T)(using resource.Context): Unit =
  val newNode = Node(resource.Box(elem), resource.Box(None))
  if isEmpty(self.borrowImmut) then self.write(list => list.head.set(Some(resource.Box(newNode))))
  else
    self.write(list =>
      newNode.next.swap(list.head)
      list.head.set(Some(resource.Box(newNode)))
    )

def pop[T](self: resource.MutRef[List[T]])(using resource.Context): Option[resource.Box[T]] =
  if isEmpty(self.borrowImmut) then None
  else
    val list = self.borrowMut
    val currentHead = self.write(list =>
      val currentHead = resource.Box[Link[T]](None)
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

def peek[T](self: resource.ImmutRef[List[T]])(using
    resource.Context
): Option[resource.ImmutRef[T]] =
  self.read(_.head.borrowImmut.read(_ match
    case None => None
    case Some(nodeBox) =>
      Some(nodeBox.borrowImmut.read(_.elem.borrowImmut))
  ))

def peekMut[T](self: resource.MutRef[List[T]])(using resource.Context): Option[resource.MutRef[T]] =
  self.read(_.head.borrowMut.read(_ match
    case None          => None
    case Some(nodeBox) =>
      // NOTE: it's wrong that it has mut borrow access to `elem` box through read method if nodeBox.
      Some(nodeBox.borrowMut.read(_.elem.borrowMut))
  ))

def isEmpty[T](self: resource.ImmutRef[List[T]])(using resource.Context): Boolean =
  self.read(_.head.borrowImmut.read(_.isEmpty))

/** One time iterator
  *
  * NOTE: The iterator gets the `resource.Context` while creation, and then use the *same* context
  * when iterating. For now the assumption is that there is only one context in the program, but in
  * overall, this my cause some bugs.
  */
def intoIter[T](self: resource.Box[List[T]])(using resource.Context): Iterator[resource.Box[T]] =
  new Iterator[resource.Box[T]] {
    // Move self to not be accessible
    val list = self.move()
    def hasNext: Boolean = !imem.isEmpty(list.borrowImmut)
    def next(): resource.Box[T] =
      pop(list.borrowMut).getOrElse(throw new NoSuchElementException("next on empty iterator"))
  }

trait Iterator[+A]:

  def hasNext: Boolean
  def next(): A

/** Immutable iterator
  *
  * NOTE: Same `resource.Context` problem as `intoIter`.
  */
def iter[T](
    self: resource.ImmutRef[List[T]]
)(using resource.Context): Iterator[resource.ImmutRef[T]] =
  new Iterator[resource.ImmutRef[T]] {
    private var current: Option[resource.ImmutRef[Node[T]]] = self.read(
      // NOTE: this could be borrowMut and nothing would have happened.
      _.head.borrowImmut.read(_.map(_.borrowImmut))
    )
    def hasNext: Boolean = current.isDefined
    def next(): resource.ImmutRef[T] =
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
    self: resource.MutRef[List[T]]
)(using resource.Context): Iterator[resource.MutRef[T]] =
  new Iterator[resource.MutRef[T]] {
    private var current: Option[resource.MutRef[Node[T]]] = self.read(
      // TODO: Should ban borrow muting while reading.
      // The question is will be automatically banned by exclusive capabilities?
      _.head.borrowMut.read(_.map(_.borrowMut))
    )
    def hasNext: Boolean = current.isDefined
    def next(): resource.MutRef[T] =
      current match
        case Some(nodeRef) =>
          nodeRef.read(node =>
            current = node.next.borrowMut.read(_.map(_.borrowMut))
            node.elem.borrowMut
          )
        case None =>
          throw new NoSuchElementException("next on empty iterator")
  }
