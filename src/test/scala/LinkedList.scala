
import language.experimental.captureChecking

class LinkedList[T, O1^](val head: imem.Box[Link[T, O1], O1] = imem.Box.newExplicit[Link[T, O1], O1](None))

object LinkedList:
  def newFromBackground[T](using imem.OwnerCarrier^) =
    def ownerInferrer[Owner^](using imem.OwnerCarrier^{Owner}) =
      new LinkedList[T, Owner]()
    ownerInferrer
end LinkedList

type Link[T, O1^] = Option[imem.Box[Node[T, O1], O1]]

class Node[T, O1^](val elem: imem.Box[T, O1], val next: imem.Box[Link[T, O1], O1])

/**
  * NOTE: All the owners are now marked as the same, for just making things compile and starting.
  */

// Implementation of List[T]
def push[T, O1^, O2^](self: imem.MutRef[LinkedList[T, O1], O2], elem: T)(using imem.Context): Unit =
  val newNode = Node(imem.Box.newExplicit[T, O1](elem), imem.Box.newExplicit[Link[T, O1], O1](None))
  if isEmpty(self.borrowImmut) then self.write(list => list.head.set(Some(imem.Box.newExplicit(newNode))))
  else
    self.write(list =>
      newNode.next.swap(list.head)
      list.head.set(Some(imem.Box.newExplicit[Node[T, O1], O1](newNode)))
    )

def pop[T, O1^, O2^, O3^](self: imem.MutRef[LinkedList[T, O1], O2])(using imem.Context): Option[imem.Box[T, O3]] =
  if isEmpty(self.borrowImmut) then None
  else
    val list = self.borrowMut
    val currentHead = self.write(list =>
      val currentHead = imem.Box.newExplicit[Link[T, O1], O2](None)
      // ?: I don't know how this will compile:
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

def peek[T, O1^, O2^](self: imem.ImmutRef[LinkedList[T, O1], O2])(using
    imem.Context
): Option[imem.ImmutRef[T, O2]] =
  self.read(_.head.borrowImmut.read(_ match
    case None => None
    case Some(nodeBox) =>
      Some(nodeBox.borrowImmut.read(_.elem.borrowImmut))
  ))

def peekMut[T, O1^, O2^](self: imem.MutRef[LinkedList[T, O1], O2])(using imem.Context): Option[imem.MutRef[T, O2]] =
  self.read(_.head.borrowMut.read(_ match
    case None          => None
    case Some(nodeBox) =>
      // NOTE: it's wrong that it has mut borrow access to `elem` box through read method if nodeBox.
      Some(nodeBox.borrowMut.read(_.elem.borrowMut))
  ))

def isEmpty[T, O1^, O2^](self: imem.ImmutRef[LinkedList[T, O1], O2])(using imem.Context): Boolean =
  self.read(_.head.borrowImmut.read(_.isEmpty))

/** One time iterator
  *
  * NOTE: The iterator gets the `resource.Context` while creation, and then use the *same* context
  * when iterating. For now the assumption is that there is only one context in the program, but in
  * overall, this my cause some bugs.
  */
def intoIter[T, O1^](self: imem.Box[LinkedList[T, O1], O1])(using imem.Context): Iterator[imem.Box[T, O1]] =
  new Iterator[imem.Box[T, O1]] {
    // Move self to not be accessible
    val list = self.move()
    def hasNext: Boolean = !isEmpty(list.borrowImmut)
    def next(): imem.Box[T, O1] =
      pop(list.borrowMut).getOrElse(throw new NoSuchElementException("next on empty iterator"))
  }

trait Iterator[+A]:

  def hasNext: Boolean
  def next(): A

/** Immutable iterator
  *
  * NOTE: Same `resource.Context` problem as `intoIter`.
  */
def iterImmut[T, O1^, O2^ >: O1](
    self: imem.ImmutRef[LinkedList[T, O1], O2]
)(using imem.Context): Iterator [imem.ImmutRef[T, O2]] =
  new Iterator [imem.ImmutRef[T, O2]] {
    private var current: Option [imem.ImmutRef[Node[T, O1], O2]] = self.read(
      // NOTE: this could be borrowMut and nothing would have happened.
      _.head.borrowImmut.read(_.map(_.borrowImmut))
    )
    def hasNext: Boolean = current.isDefined
    def next(): imem.ImmutRef[T, O2] =
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
    self: imem.MutRef[LinkedList[T, Owner], Owner]
)(using imem.Context): Iterator [imem.MutRef[T, Owner]] =
  new Iterator [imem.MutRef[T, Owner]] {
    private var current: Option [imem.MutRef[Node[T, Owner], Owner]] = self.read(
      // TODO: Should ban borrow muting while reading.
      // The question is will be automatically banned by exclusive capabilities?
      _.head.borrowMut.read(_.map(_.borrowMut))
    )
    def hasNext: Boolean = current.isDefined
    def next(): imem.MutRef[T, Owner] =
      current match
        case Some(nodeRef) =>
          nodeRef.read(node =>
            current = node.next.borrowMut.read(_.map(_.borrowMut))
            node.elem.borrowMut
          )
        case None =>
          throw new NoSuchElementException("next on empty iterator")
  }
