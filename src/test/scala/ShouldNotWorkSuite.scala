import scala.compiletime.ops.int
class ResourceShouldNotWorkSuite extends munit.FunSuite {
  test("should not be able to mutate, while reading it") {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.Box[BoxedInteger](BoxedInteger(42));

    val immutRef = myVal.borrowImmut
    myVal.borrowMut.write(_.value = 12)
    intercept[IllegalStateException] {
      immutRef.read(_ => ())
    }
  }

  test("borrows should be invalidated after moving") {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.Box[BoxedInteger](BoxedInteger(42));
    val immutRef = myVal.borrowImmut

    // TODO: for now no moving runtime/compile time effect is defined, so the following line will have no effect in the
    // stacked borrows. Should track moves in at least runtime or compile time.
    val myOtherVal = myVal

    // FIXME: due to explained reason, it will evaluate fine.
    // intercept[IllegalStateException] {
    immutRef.read(_ => ())
    // }
  }
}

class ListShouldNotWorkSuite extends munit.FunSuite {

  test("should not be able to push while peeking immutably") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)

    // TODO: Although `res` is a immutable reference, there is no connection between `res` and `list`, meaning
    // mutating `list` does not affect `res`. The connection should be established at least in runtime or compile-time.
    val res = imem.peek(list.borrowImmut)

    // FIXME: due to explained reason, it will evaluate fine.
    // intercept[IllegalStateException] {
    imem.push(list.borrowMut, 2)
    res.get.read(_ => ()) // idle read
    // }
  }

  test("should not be able to push while peeking mutably") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)

    // TODO: Although `res` is a mutable reference, there is no connection between `res` and `list`, meaning
    // mutating `list` does not affect `res`. The connection should be established at in runtime or compile-time.
    val res = imem.peekMut(list.borrowMut)

    // FIXME: due to explained reason, it will evaluate fine.
    // intercept[IllegalStateException] {
    imem.push(list.borrowMut, 2)
    res.get.read(_ => ()) // idle read
    // }
  }

  test("should not be able to push/pop while iterating") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    // TODO: Like mentioned above, the internal reference in `iter` has no connection to `list`, because
    // It's reference to a `Node`, not the `List`. so Re-borrowing and modifying `list` does not affect `iter`.
    // This connection should be established at least in runtime or compile-time.
    val iter = imem.iter(list.borrowImmut)
    // FIXME: due to explained reason, it will evaluate fine.
    // intercept[IllegalStateException] {
    imem.pop(list.borrowMut)
    iter.next()
    // }
  }

  test("should not be able to peek list after it is consumed") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    // TODO: for now no moving runtime/compile time effect is defined, so the following line will have no effect in the
    // stacked borrows. Should track moves in at least runtime or compile time.
    val iter = imem.intoIter(list)

    // FIXME: due to explained reason, it will evaluate fine.
    // intercept[IllegalStateException] {
    imem.peek(list.borrowImmut)
    // }
  }

  test("should not be able to re-consume list after it is consumed") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    // TODO: for now no moving runtime/compile time effect is defined, so the following line will have no effect in the
    // stacked borrows. Should track moves in at least runtime or compile time.
    val iter = imem.intoIter(list)

    // FIXME: due to explained reason, it will evaluate fine.
    // intercept[IllegalStateException] {
    imem.intoIter(list)
    // }
  }
}
