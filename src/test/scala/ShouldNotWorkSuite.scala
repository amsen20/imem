import scala.compiletime.ops.int
class ResourceShouldNotWorkSuite extends munit.FunSuite {
  test("should not be able to write to a reference, by reading it") {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.Box[BoxedInteger](BoxedInteger(42))

    // TODO: For now, there is no difference between, `read` and `write` methods. Both can mutate the value. This
    // Should be changed, at least in runtime or compile-time, mutation through `read`s should not be allowed.
    // FIXME: Due to explained reason, it will evaluate fine.
    // intercept[IllegalStateException] {
    myVal.borrowImmut.read(v => v.value = 12)
    // }
  }

  test("should not be able to escape a value through `read`/`write` methods of a reference") {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.Box[BoxedInteger](BoxedInteger(42))

    // TODO: For now, values can be leaked though `read` and `write` methods. This should be changed, at least in
    // runtime or compile-time.
    // FIXME: Due to explained reason, it will evaluate fine.
    // intercept[Exception] {
    myVal.borrowImmut.read(v => v).value
    // }
  }

  test("should not be able to mutate, while reading it") {
    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.Box[BoxedInteger](BoxedInteger(42))

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

    // TODO: For now no moving runtime/compile time effect is defined, so the following line will have no effect in the
    // stacked borrows. Should track moves in at least runtime or compile time.
    val myOtherVal = myVal

    // FIXME: Due to explained reason, it will evaluate fine.
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

    // FIXME: Due to explained reason, it will evaluate fine.
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

    // FIXME: Due to explained reason, it will evaluate fine.
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

    // FIXME: Due to explained reason, it will evaluate fine.
    // intercept[IllegalStateException] {
    imem.peek(list.borrowImmut)
    // }
  }

  test("should not be able to re-consume list after it is consumed") {
    val list = imem.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    // TODO: For now no moving runtime/compile time effect is defined, so the following line will have no effect in the
    // stacked borrows. Should track moves in at least runtime or compile time.
    val iter = imem.intoIter(list)

    // FIXME: Due to explained reason, it will evaluate fine.
    // intercept[IllegalStateException] {
    imem.intoIter(list)
    // }
  }
}
