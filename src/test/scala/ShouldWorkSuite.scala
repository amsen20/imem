class ResourceShouldWorkSuite extends munit.FunSuite {
  test(
    "basics: borrow, mutate and read"
  ) {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.Box[BoxedInteger](BoxedInteger(42));

    assertEquals(myVal.borrowImmut.value, BoxedInteger(42));
    myVal.borrowMut.value.value = 12;
    assertEquals(myVal.borrowImmut.value, BoxedInteger(12));
  }
}

class ListShouldWorkSuite extends munit.FunSuite {

  test("basics: push and pop") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())

    val mutList = list.borrowMut

    // Check empty list behaves right
    assertEquals(imem.pop(mutList), None)

    // Populate list
    imem.push(mutList, 1)
    imem.push(mutList, 2)
    imem.push(mutList, 3)

    // Check normal removal
    assertEquals(imem.pop(mutList), Some(3))
    assertEquals(imem.pop(mutList), Some(2))

    // Push some more just to make sure nothing's corrupted
    imem.push(mutList, 4)
    imem.push(mutList, 5)

    // Check normal removal
    assertEquals(imem.pop(mutList), Some(5))
    assertEquals(imem.pop(mutList), Some(4))

    // Check exhaustion
    assertEquals(imem.pop(mutList), Some(1))
    assertEquals(imem.pop(mutList), None)
  }

  test("peek and peekMut") {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val list = imem.Box[imem.List[BoxedInteger]](imem.List[BoxedInteger]())
    assertEquals(imem.peek(list.borrowImmut), None)
    assertEquals(imem.peekMut(list.borrowMut), None)

    imem.push(list.borrowMut, BoxedInteger(1))
    imem.push(list.borrowMut, BoxedInteger(2))
    imem.push(list.borrowMut, BoxedInteger(3))

    // TODO: this is just an `Option[T]` while it should be `Option[ImmutRef[T]]`
    assertEquals(imem.peek(list.borrowImmut), Some(BoxedInteger(3)))
    // TODO: this is just an `Option[T]` while it should be `Option[MutRef[T]]`
    assertEquals(imem.peekMut(list.borrowMut), Some(BoxedInteger(3)))

    // Modify the value using the mutable reference from peekMut

    imem.peekMut(list.borrowMut).foreach(boxedInt => boxedInt.value = 42)

    assertEquals(imem.peek(list.borrowImmut), Some(BoxedInteger(42)))
    assertEquals(imem.pop(list.borrowMut), Some(BoxedInteger(42)))
    assertEquals(imem.peek(list.borrowImmut), Some(BoxedInteger(2)))
  }

  test("into_iter: consuming iterator") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    val iter = imem.intoIter(list)
    assertEquals(iter.next(), 3)
    assertEquals(iter.next(), 2)
    assertEquals(iter.next(), 1)
    assert(!iter.hasNext)
  }

  test("iter: non-consuming immutable iterator") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    val iter = imem.iter(list.borrowImmut)
    assertEquals(iter.next(), 3)
    assertEquals(iter.next(), 2)

    // Check that the original list is unchanged in the process
    assertEquals(imem.peek(list.borrowImmut), Some(3))

    assertEquals(iter.next(), 1)
    assert(!iter.hasNext)

    // Check that the original list is unchanged
    assertEquals(imem.peek(list.borrowImmut), Some(3))
  }

  test("iter_mut: non-consuming mutable iterator") {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val list = imem.Box[imem.List[BoxedInteger]](imem.List[BoxedInteger]())

    imem.push(list.borrowMut, BoxedInteger(1))
    imem.push(list.borrowMut, BoxedInteger(2))
    imem.push(list.borrowMut, BoxedInteger(3))

    // Use the mutable iterator to modify the elements in the list
    imem.iterMut(list.borrowMut).foreach { elem =>
      elem.value = elem.value * 10
    }

    // Check that the list contains the new, modified values
    assertEquals(imem.pop(list.borrowMut), Some(BoxedInteger(30)))
    assertEquals(imem.pop(list.borrowMut), Some(BoxedInteger(20)))
    assertEquals(imem.pop(list.borrowMut), Some(BoxedInteger(10)))
    assertEquals(imem.pop(list.borrowMut), None)
  }
}
