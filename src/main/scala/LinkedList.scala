package imem

class List[T](var head: Link[T] = None)

type Link[T] = Option[Box[Node[T]]]

class Node[T](val elem: T, var next: Link[T])

// Implementation of List[T]
def push[T](self: MutRef[List[T]], elem: T): Unit = {
  val newNode = Node(elem, self.getValue.head)
  self.getValue.head = Some(Box(newNode))
}

def pop[T](self: MutRef[List[T]]): Option[T] = {
  self.getValue.head match {
    case None => None
    case Some(nodeBox) =>
      val node = nodeBox.borrowImmut
      self.getValue.head = node.getValue.next
      Some(node.getValue.elem)
  }
}

def peek[T](self: ImmutRef[List[T]]): Option[T] = {
  self.getValue.head match {
    case None => None
    case Some(nodeBox) =>
      Some(nodeBox.borrowImmut.getValue.elem)
  }
}

def peekMut[T](self: MutRef[List[T]]): Option[T] = {
  self.getValue.head match {
    case None => None
    case Some(nodeBox) =>
      Some(nodeBox.borrowMut.getValue.elem)
  }
}

def isEmpty[T](self: ImmutRef[List[T]]): Boolean = {
  self.getValue.head.isEmpty
}

// One time iterator
def intoIter[T](self: Box[List[T]]): Iterator[T] = new Iterator[T] {
  def hasNext: Boolean = !imem.isEmpty(self.borrowImmut)
  def next(): T =
    pop(self.borrowMut).getOrElse(throw new NoSuchElementException("next on empty iterator"))
}

// Immutable iterator
def iter[T](self: ImmutRef[List[T]]): Iterator[T] = new Iterator[T] {
  private var current: Option[ImmutRef[Node[T]]] = self.getValue.head.map(_.borrowImmut)
  def hasNext: Boolean = current.isDefined
  def next(): T = {
    current match {
      case Some(node) =>
        // ! Anything returned from getValue should be immutable reference.
        val elem = node.getValue.elem
        current = node.getValue.next.map(_.borrowImmut)
        // ! This should be an immutable reference, not just the element.
        elem
      case None =>
        throw new NoSuchElementException("next on empty iterator")
    }
  }
}

// Mutable iterator
def iterMut[T](self: MutRef[List[T]]): Iterator[T] = new Iterator[T] {
  private var current: Option[MutRef[Node[T]]] = self.getValue.head.map(_.borrowMut)
  def hasNext: Boolean = current.isDefined
  def next(): T = {
    current match {
      case Some(node) =>
        // ! Anything returned from getValue should be mutable reference.
        val elem = node.getValue.elem
        current = node.getValue.next.map(_.borrowMut)
        // ! This should be a mutable reference, not just the element.
        elem
      case None =>
        throw new NoSuchElementException("next on empty iterator")
    }
  }
}
