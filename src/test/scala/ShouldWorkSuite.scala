class ResourceShouldWorkSuite extends munit.FunSuite:
  test("basics: borrow, mutate and read") {
    given imem.resource.Context = new imem.resource.TemporaryContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.resource.Box[BoxedInteger](BoxedInteger(42))

    assert(myVal.borrowImmut.read(_ == BoxedInteger(42)))
    myVal.borrowMut.write(_.value = 12)
    assert(myVal.borrowImmut.read(_ == BoxedInteger(12)))
  }
end ResourceShouldWorkSuite

class ListShouldWorkSuite extends munit.FunSuite:

  test("basics: push and pop") {
    given imem.resource.Context = new imem.resource.TemporaryContext

    val list = imem.resource.Box[imem.List[Int]](imem.List[Int]())

    val mutList = list.borrowMut

    // Check empty list behaves right
    assertEquals(imem.pop(mutList), None)

    // Populate list
    imem.push(mutList, 1)
    imem.push(mutList, 2)
    imem.push(mutList, 3)

    // Check normal removal
    assert(imem.pop(mutList).map(_.borrowImmut.read(_ == 3)).getOrElse(false))
    assert(imem.pop(mutList).map(_.borrowImmut.read(_ == 2)).getOrElse(false))

    // Push some more just to make sure nothing's corrupted
    imem.push(mutList, 4)
    imem.push(mutList, 5)

    // Check normal removal
    assert(imem.pop(mutList).map(_.borrowImmut.read(_ == 5)).getOrElse(false))
    assert(imem.pop(mutList).map(_.borrowImmut.read(_ == 4)).getOrElse(false))

    // Check exhaustion
    assert(imem.pop(mutList).map(_.borrowImmut.read(_ == 1)).getOrElse(false))
    assert(imem.pop(mutList).isEmpty)
  }

  test("peek and peekMut") {
    given imem.resource.Context = new imem.resource.TemporaryContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val list = imem.resource.Box[imem.List[BoxedInteger]](imem.List[BoxedInteger]())
    assertEquals(imem.peek(list.borrowImmut), None)
    assertEquals(imem.peekMut(list.borrowMut), None)

    imem.push(list.borrowMut, BoxedInteger(1))
    imem.push(list.borrowMut, BoxedInteger(2))
    imem.push(list.borrowMut, BoxedInteger(3))

    assert(imem.peek(list.borrowImmut).map(_.read(_ == BoxedInteger(3))).getOrElse(false))

    // Modify the value using the mutable reference from peekMut

    imem.peekMut(list.borrowMut).foreach(ref => ref.write(_.value = 42))

    assert(imem.peek(list.borrowImmut).map(_.read(_ == BoxedInteger(42))).getOrElse(false))
    assert(imem.pop(list.borrowMut).map(_.borrowMut.read(_ == BoxedInteger(42))).getOrElse(false))

    assert(imem.pop(list.borrowMut).map(_.borrowMut.read(_ == BoxedInteger(2))).getOrElse(false))
  }

  test("into_iter: consuming iterator") {
    given imem.resource.Context = new imem.resource.TemporaryContext

    val list = imem.resource.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    val iter = imem.intoIter(list)
    assert(iter.next().borrowImmut.read(_ == 3))
    assert(iter.next().borrowImmut.read(_ == 2))
    assert(iter.next().borrowImmut.read(_ == 1))
    assert(!iter.hasNext)
  }

  test("iter: non-consuming immutable iterator") {
    given imem.resource.Context = new imem.resource.TemporaryContext

    val list = imem.resource.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    val iter = imem.iter(list.borrowImmut)
    assert(iter.next().read(_ == 3))
    assert(iter.next().read(_ == 2))

    // Check that the original list is unchanged in the process
    assert(imem.peek(list.borrowImmut).map(_.read(_ == 3)).getOrElse(false))

    assert(iter.next().read(_ == 1))
    assert(!iter.hasNext)

    // Check that the original list is unchanged
    assert(imem.peek(list.borrowImmut).map(_.read(_ == 3)).getOrElse(false))
  }

  test("iter_mut: non-consuming mutable iterator") {
    given imem.resource.Context = new imem.resource.TemporaryContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val list = imem.resource.Box[imem.List[BoxedInteger]](imem.List[BoxedInteger]())

    imem.push(list.borrowMut, BoxedInteger(1))
    imem.push(list.borrowMut, BoxedInteger(2))
    imem.push(list.borrowMut, BoxedInteger(3))

    // Use the mutable iterator to modify the elements in the list
    imem.iterMut(list.borrowMut).foreach { elemRef =>
      elemRef.write(elem => elem.value = elem.value * 10)
    }

    // Check that the list contains the new, modified values
    assert(imem.pop(list.borrowMut).map(_.borrowImmut.read(_ == BoxedInteger(30))).getOrElse(false))
    assert(imem.pop(list.borrowMut).map(_.borrowImmut.read(_ == BoxedInteger(20))).getOrElse(false))
    assert(imem.pop(list.borrowMut).map(_.borrowImmut.read(_ == BoxedInteger(10))).getOrElse(false))
    assert(imem.pop(list.borrowMut).isEmpty)
  }
end ListShouldWorkSuite
