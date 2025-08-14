package imem

class List[T](var head: Link[T] = None)

type Link[T] = Option[Box[Node[T]]]

class Node[T](val elem: Box[T], var next: Link[T])

// Implementation of List[T]
def push[T](self: MutRef[List[T]], elem: T): Unit =
  /** TODO it's escaping `head` and should be avoided but for now it's like this, so when escape
    * prevention happened a compile error should appear around here.
    */
  val newNode = Node(Box(elem), self.read(_.head)) // TODO `head` is escaping.
  self.write(_.head = Some(Box(newNode)))

def pop[T](self: MutRef[List[T]]): Option[Box[T]] =
  self.read(_.head match {
    case None          => None
    case Some(nodeBox) =>
      // TODO it's weird that it is being borrowed immutably but, a field of it (.elem) is moved out.
      nodeBox.borrowImmut.read(node =>
        // TODO it looks weird, first it's borrowed for read, and then it's borrowed for write.
        self.write(_.head = node.next)
        Some(node.elem)
      )
  })

def peek[T](self: ImmutRef[List[T]]): Option[ImmutRef[T]] =
  self.read(_.head match
    case None => None
    case Some(nodeBox) =>
      Some(nodeBox.borrowImmut.read(_.elem.borrowImmut))
  )

def peekMut[T](self: MutRef[List[T]]): Option[MutRef[T]] =
  self.read(_.head match
    case None          => None
    case Some(nodeBox) =>
      // TODO it's wrong that it has mut borrow access to `elem` box through read method if nodeBox.
      Some(nodeBox.borrowMut.read(_.elem.borrowMut))
  )

def isEmpty[T](self: ImmutRef[List[T]]): Boolean =
  self.read(_.head.isEmpty)

// One time iterator
def intoIter[T](self: Box[List[T]]): Iterator[Box[T]] = new Iterator[Box[T]] {
  def hasNext: Boolean = !imem.isEmpty(self.borrowImmut)
  def next(): Box[T] =
    pop(self.borrowMut).getOrElse(throw new NoSuchElementException("next on empty iterator"))
}

// Immutable iterator
def iter[T](self: ImmutRef[List[T]]): Iterator[ImmutRef[T]] = new Iterator[ImmutRef[T]] {
  private var current: Option[ImmutRef[Node[T]]] = self.read(
    // TODO this could be borrowMut and nothing would have happened.
    _.head.map(_.borrowImmut)
  )
  def hasNext: Boolean = current.isDefined
  def next(): ImmutRef[T] =
    current match
      case Some(nodeRef) =>
        nodeRef.read(node =>
          current = node.next.map(_.borrowImmut)
          // TODO this could be borrowMut and nothing would have happened.
          node.elem.borrowImmut
        )
      case None =>
        throw new NoSuchElementException("next on empty iterator")

}

// Mutable iterator
def iterMut[T](self: MutRef[List[T]]): Iterator[MutRef[T]] = new Iterator[MutRef[T]] {
  private var current: Option[MutRef[Node[T]]] = self.read(
    _.head.map(_.borrowMut)
  )
  def hasNext: Boolean = current.isDefined
  def next(): MutRef[T] =
    current match
      case Some(nodeRef) =>
        nodeRef.read(node =>
          current = node.next.map(_.borrowMut)
          node.elem.borrowMut
        )
      case None =>
        throw new NoSuchElementException("next on empty iterator")
}
