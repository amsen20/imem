package imem

class List[T](val head: Box[Link[T]] = Box[Link[T]](None))

type Link[T] = Option[Box[Node[T]]]

class Node[T](val elem: Box[T], val next: Box[Link[T]])

// Implementation of List[T]
def push[T](self: MutRef[List[T]], elem: T): Unit =
  val newNode = Node(Box(elem), Box(None))
  if isEmpty(self.borrowImmut) then self.write(list => list.head.set(Some(Box(newNode))))
  else
    self.write(list =>
      newNode.next.swap(list.head)
      list.head.set(Some(Box(newNode)))
    )

def pop[T](self: MutRef[List[T]]): Option[Box[T]] =
  if isEmpty(self.borrowImmut) then None
  else
    val list = self.borrowMut
    val currentHead = self.write(list =>
      val currentHead = Box[Link[T]](None)
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

def peek[T](self: ImmutRef[List[T]]): Option[ImmutRef[T]] =
  self.read(_.head.borrowImmut.read(_ match
    case None => None
    case Some(nodeBox) =>
      Some(nodeBox.borrowImmut.read(_.elem.borrowImmut))
  ))

def peekMut[T](self: MutRef[List[T]]): Option[MutRef[T]] =
  self.read(_.head.borrowMut.read(_ match
    case None          => None
    case Some(nodeBox) =>
      // NOTE: it's wrong that it has mut borrow access to `elem` box through read method if nodeBox.
      Some(nodeBox.borrowMut.read(_.elem.borrowMut))
  ))

def isEmpty[T](self: ImmutRef[List[T]]): Boolean =
  self.read(_.head.borrowImmut.read(_.isEmpty))

// One time iterator
def intoIter[T](self: Box[List[T]]): Iterator[Box[T]] = new Iterator[Box[T]] {
  // Move self to not be accessible
  val list = self.move()
  def hasNext: Boolean = !imem.isEmpty(list.borrowImmut)
  def next(): Box[T] =
    pop(list.borrowMut).getOrElse(throw new NoSuchElementException("next on empty iterator"))
}

// Immutable iterator
def iter[T](self: ImmutRef[List[T]]): Iterator[ImmutRef[T]] = new Iterator[ImmutRef[T]] {
  private var current: Option[ImmutRef[Node[T]]] = self.read(
    // NOTE: this could be borrowMut and nothing would have happened.
    _.head.borrowImmut.read(_.map(_.borrowImmut))
  )
  def hasNext: Boolean = current.isDefined
  def next(): ImmutRef[T] =
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

// Mutable iterator
def iterMut[T](self: MutRef[List[T]]): Iterator[MutRef[T]] = new Iterator[MutRef[T]] {
  private var current: Option[MutRef[Node[T]]] = self.read(
    // TODO: Should ban borrow muting while reading.
    // The question is will be automatically banned by exclusive capabilities?
    _.head.borrowMut.read(_.map(_.borrowMut))
  )
  def hasNext: Boolean = current.isDefined
  def next(): MutRef[T] =
    current match
      case Some(nodeRef) =>
        nodeRef.read(node =>
          current = node.next.borrowMut.read(_.map(_.borrowMut))
          node.elem.borrowMut
        )
      case None =>
        throw new NoSuchElementException("next on empty iterator")
}
