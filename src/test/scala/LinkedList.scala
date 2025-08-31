
import language.experimental.captureChecking

class LinkedList[T, Owner^](val head: imem.Box[Link[T, Owner], Owner] = imem.Box.newExplicit[Link[T, Owner], Owner](None))

object LinkedList:
  def newFromBackground[T](using imem.OwnerCarrier^) =
    def ownerInferrer[Owner^](using imem.OwnerCarrier^{Owner}) =
      new LinkedList[T, Owner]()
    ownerInferrer
end LinkedList

type Link[T, Owner^] = Option[imem.Box[Node[T, Owner], Owner]]

class Node[T, Owner^](val elem: imem.Box[T, Owner], val next: imem.Box[Link[T, Owner], Owner])

/**
  * NOTE: All the owners are now marked as the same, for just making things compile and starting.
  */

// Implementation of List[T]
def push[T, Owner^](self: imem.MutRef[LinkedList[T, Owner]], elem: T)(using imem.Context): Unit =
  val newNode = Node(imem.Box.newExplicit[T, Owner](elem), imem.Box.newExplicit[Link[T, Owner], Owner](None))
  if isEmpty(self.borrowImmut) then self.write(list => list.head.set(Some(imem.Box.newExplicit(newNode))))
  else
    self.write(list =>
      newNode.next.swap(list.head)
      list.head.set(Some(imem.Box.newExplicit[Node[T, Owner], Owner](newNode)))
    )

def pop[T, Owner^](self: imem.MutRef[LinkedList[T, Owner]])(using imem.Context): Option[imem.Box[T, Owner]] =
  if isEmpty(self.borrowImmut) then None
  else
    val list = self.borrowMut
    val currentHead = self.write(list =>
      val currentHead = imem.Box.newExplicit[Link[T, Owner], Owner](None)
      // NOTE: I don't know how this will compile:
      // currentHead.swap(list.head)
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

def peek[T, Owner^](self: imem.ImmutRef[LinkedList[T, Owner]])(using
    imem.Context
): Option[imem.ImmutRef[T]] =
  self.read(_.head.borrowImmut.read(_ match
    case None => None
    case Some(nodeBox) =>
      Some(nodeBox.borrowImmut.read(_.elem.borrowImmut))
  ))

def peekMut[T, Owner^](self: imem.MutRef[LinkedList[T, Owner]])(using imem.Context): Option [imem.MutRef[T]] =
  self.read(_.head.borrowMut.read(_ match
    case None          => None
    case Some(nodeBox) =>
      // NOTE: it's wrong that it has mut borrow access to `elem` box through read method if nodeBox.
      Some(nodeBox.borrowMut.read(_.elem.borrowMut))
  ))

def isEmpty[T, Owner^](self: imem.ImmutRef[LinkedList[T, Owner]])(using imem.Context): Boolean =
  self.read(_.head.borrowImmut.read(_.isEmpty))

/** One time iterator
  *
  * NOTE: The iterator gets the `resource.Context` while creation, and then use the *same* context
  * when iterating. For now the assumption is that there is only one context in the program, but in
  * overall, this my cause some bugs.
  */
def intoIter[T, Owner^](self: imem.Box[LinkedList[T, Owner], Owner])(using imem.Context): Iterator[imem.Box[T, Owner]] =
  new Iterator[imem.Box[T, Owner]] {
    // Move self to not be accessible
    val list = self.move()
    def hasNext: Boolean = !isEmpty(list.borrowImmut)
    def next(): imem.Box[T, Owner] =
      pop(list.borrowMut).getOrElse(throw new NoSuchElementException("next on empty iterator"))
  }

trait Iterator[+A]:

  def hasNext: Boolean
  def next(): A

/** Immutable iterator
  *
  * NOTE: Same `resource.Context` problem as `intoIter`.
  */
def iterImmut[T, Owner^](
    self: imem.ImmutRef[LinkedList[T, Owner]]
)(using imem.Context): Iterator [imem.ImmutRef[T]] =
  new Iterator [imem.ImmutRef[T]] {
    private var current: Option [imem.ImmutRef[Node[T, Owner]]] = self.read(
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
def iterMut[T, Owner^](
    self: imem.MutRef[LinkedList[T, Owner]]
)(using imem.Context): Iterator [imem.MutRef[T]] =
  new Iterator [imem.MutRef[T]] {
    private var current: Option [imem.MutRef[Node[T, Owner]]] = self.read(
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
