import scala.compiletime.ops.int
class ResourceShouldNotWorkSuite extends munit.FunSuite {
  test("should not be able to mutate, while reading it") {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.Box[BoxedInteger](BoxedInteger(42));

    val immutRef = myVal.borrowImmut
    myVal.borrowMut.getValue.value = 12;
    intercept[IllegalStateException] {
      immutRef.getValue
    }
  }

  test("borrows should be invalidated after moving") {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.Box[BoxedInteger](BoxedInteger(42));
    val immutRef = myVal.borrowImmut

    val myOtherVal = myVal

    intercept[IllegalStateException] {
      immutRef.getValue
    }
  }
}

class ListShouldNotWorkSuite extends munit.FunSuite {

  test("should not be able to push/pop while peeking (mut or immut)") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())

    // try to peek while pushing
    {
      // TODO: this should be an immutable reference, not just the element.
      val res = imem.peek(list.borrowImmut)
      intercept[IllegalStateException] {
        imem.push(list.borrowMut, 1)
        res.exists(_ => true) // force evaluation
      }
    }

    // try to peekMut while pushing
    {
      // TODO: this should be a mutable reference, not just the element.
      val res = imem.peekMut(list.borrowMut)
      intercept[IllegalStateException] {
        imem.push(list.borrowMut, 1)
        res.exists(_ => true) // force evaluation
      }
    }
  }

  test("should not be able to push/pop while iterating") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    // try to pop while iterating
    val iter = imem.iter(list.borrowImmut)
    intercept[IllegalStateException] {
      iter.next()
      imem.pop(list.borrowMut)
    }

    // try to push while iterating
    intercept[IllegalStateException] {
      iter.next()
      imem.push(list.borrowMut, 4)
    }
  }

  test("should not be able to peek list after it is consumed") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    val iter = imem.intoIter(list)

    // after consuming the list, peek should not work
    intercept[IllegalStateException] {
      imem.peek(list.borrowImmut)
    }
    // after consuming the list, re-consuming it should not work
    intercept[IllegalStateException] {
      imem.intoIter(list)
    }

    assertEquals(iter.next(), 3)
    assertEquals(iter.next(), 2)
    assertEquals(iter.next(), 1)
  }
}
