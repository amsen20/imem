import language.experimental.captureChecking

class ResourceShouldWorkSuite extends munit.FunSuite:
  test("basics: borrow, mutate and read") {
    given imem.Context = new imem.DefaultContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.Box[BoxedInteger](BoxedInteger(42))

    assert(myVal.borrowImmut.read(_ == BoxedInteger(42)))
    myVal.borrowMut.write(_.value = 12)
    assert(myVal.borrowImmut.read(_ == BoxedInteger(12)))
  }
end ResourceShouldWorkSuite

class ListShouldWorkSuite extends munit.FunSuite:

  test("basics: push and pop") {
    given imem.Context = new imem.DefaultContext

    val list = imem.Box[LinkedList[Int]](LinkedList[Int]())

    val mutList = list.borrowMut

    // Check empty list behaves right
    assertEquals(pop(mutList), None)

    // Populate list
    push(mutList, 1)
    push(mutList, 2)
    push(mutList, 3)

    // Check normal removal
    assert(pop(mutList).map(_.borrowImmut.read(_ == 3)).getOrElse(false))
    assert(pop(mutList).map(_.borrowImmut.read(_ == 2)).getOrElse(false))

    // Push some more just to make sure nothing's corrupted
    push(mutList, 4)
    push(mutList, 5)

    // Check normal removal
    assert(pop(mutList).map(_.borrowImmut.read(_ == 5)).getOrElse(false))
    assert(pop(mutList).map(_.borrowImmut.read(_ == 4)).getOrElse(false))

    // Check exhaustion
    assert(pop(mutList).map(_.borrowImmut.read(_ == 1)).getOrElse(false))
    assert(pop(mutList).isEmpty)
  }

  test("peek and peekMut") {
    given imem.Context = new imem.DefaultContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val list = imem.Box[LinkedList[BoxedInteger]](LinkedList[BoxedInteger]())
    assertEquals(peek(list.borrowImmut), None)
    assertEquals(peekMut(list.borrowMut), None)

    push(list.borrowMut, BoxedInteger(1))
    push(list.borrowMut, BoxedInteger(2))
    push(list.borrowMut, BoxedInteger(3))

    assert(peek(list.borrowImmut).map(_.read(_ == BoxedInteger(3))).getOrElse(false))

    // Modify the value using the mutable reference from peekMut

    peekMut(list.borrowMut).foreach(ref => ref.write(_.value = 42))

    assert(peek(list.borrowImmut).map(_.read(_ == BoxedInteger(42))).getOrElse(false))
    assert(pop(list.borrowMut).map(_.borrowMut.read(_ == BoxedInteger(42))).getOrElse(false))
    assert(pop(list.borrowMut).map(_.borrowMut.read(_ == BoxedInteger(2))).getOrElse(false))
  }

  test("into_iter: consuming iterator") {
    given imem.Context = new imem.DefaultContext

    val list = imem.Box[LinkedList[Int]](LinkedList[Int]())
    push(list.borrowMut, 1)
    push(list.borrowMut, 2)
    push(list.borrowMut, 3)

    val iter = intoIter(list)
    assert(iter.next().borrowImmut.read(_ == 3))
    assert(iter.next().borrowImmut.read(_ == 2))
    assert(iter.next().borrowImmut.read(_ == 1))
    assert(!iter.hasNext)
  }

  test("iter: non-consuming immutable iterator") {
    given imem.Context = new imem.DefaultContext

    val list = imem.Box[LinkedList[Int]](LinkedList[Int]())
    push(list.borrowMut, 1)
    push(list.borrowMut, 2)
    push(list.borrowMut, 3)

    val iter = iterImmut(list.borrowImmut)
    assert(iter.next().read(_ == 3))
    assert(iter.next().read(_ == 2))

    // Check that the original list is unchanged in the process
    assert(peek(list.borrowImmut).map(_.read(_ == 3)).getOrElse(false))

    assert(iter.next().read(_ == 1))
    assert(!iter.hasNext)

    // Check that the original list is unchanged
    assert(peek(list.borrowImmut).map(_.read(_ == 3)).getOrElse(false))
  }

  test("iter_mut: non-consuming mutable iterator") {
    given imem.Context = new imem.DefaultContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val list = imem.Box[LinkedList[BoxedInteger]](LinkedList[BoxedInteger]())

    push(list.borrowMut, BoxedInteger(1))
    push(list.borrowMut, BoxedInteger(2))
    push(list.borrowMut, BoxedInteger(3))

    // Use the mutable iterator to modify the elements in the list
    // NOTE: The reason that it's a manual `while` and not `foreach` is that we are using `imem.Iterator` here
    // not `Iterator`.
    val iter = iterMut(list.borrowMut)
    while (iter.hasNext) {
      val elemRef = iter.next()
      elemRef.write(elem => elem.value = elem.value * 10)
    }

    // Check that the list contains the new, modified values
    assert(pop(list.borrowMut).map(_.borrowImmut.read(_ == BoxedInteger(30))).getOrElse(false))
    assert(pop(list.borrowMut).map(_.borrowImmut.read(_ == BoxedInteger(20))).getOrElse(false))
    assert(pop(list.borrowMut).map(_.borrowImmut.read(_ == BoxedInteger(10))).getOrElse(false))
    assert(pop(list.borrowMut).isEmpty)
  }
end ListShouldWorkSuite
