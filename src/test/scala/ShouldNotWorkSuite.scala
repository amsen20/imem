import scala.compiletime.ops.int
class ResourceShouldNotWorkSuite extends munit.FunSuite {
  test("should not be able to call the Box default constructor") {
    given imem.resource.Context = new imem.resource.DefaultContext

    // TODO: Should not compile, should make the constructor private.
    // FIXME: Due to explained reason, it will evaluate fine.
    intercept[Exception] {
      new imem.resource.Box[Int]()
    }
  }

  test("box should not be able to assigned to a variable") {
    given imem.resource.Context = new imem.resource.DefaultContext

    // TODO: A `Box` type should not be able to be assigned to a variable because it forces the user to use `Box`'s
    // methods to set and reset it. Using them we can keep track of the ownership state.
    // FIXME: Due to explained reason, it will evaluate fine.
    intercept[Exception] {
      var myVal = imem.resource.Box[Int](42)
    }
  }

  test("should not be able to write to a reference, by reading it") {
    given imem.resource.Context = new imem.resource.DefaultContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.resource.Box[BoxedInteger](BoxedInteger(42))

    // TODO: For now, there is no difference between, `read` and `write` methods. Both can mutate the value. This
    // Should be changed, at least in runtime or compile-time, mutation through `read`s should not be allowed.
    // FIXME: Due to explained reason, it will evaluate fine.
    intercept[IllegalStateException] {
      myVal.borrowImmut.read(v => v.value = 12)
    }
  }

  test("should not be able to escape a value through `read`/`write` methods of a reference") {
    given imem.resource.Context = new imem.resource.DefaultContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.resource.Box[BoxedInteger](BoxedInteger(42))

    // TODO: For now, values can be leaked though `read` and `write` methods. This should be changed, at least in
    // runtime or compile-time.
    // FIXME: Due to explained reason, it will evaluate fine.
    intercept[Exception] {
      myVal.borrowImmut.read(v => v).value
    }
  }

  test("should not be able to mutate, while reading it") {
    given imem.resource.Context = new imem.resource.DefaultContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.resource.Box[BoxedInteger](BoxedInteger(42))

    val immutRef = myVal.borrowImmut
    myVal.borrowMut.write(_.value = 12)
    intercept[IllegalStateException] {
      immutRef.read(_ => ())
    }
  }

  test("borrows should be invalidated after moving") {
    given imem.resource.Context = new imem.resource.DefaultContext

    // A way to express a mutable integer.
    case class BoxedInteger(var value: Int)
    val myVal = imem.resource.Box[BoxedInteger](BoxedInteger(42));
    val immutRef = myVal.borrowImmut

    // TODO: For now no moving runtime/compile time effect is defined, so the following line will have no effect in the
    // stacked borrows. Should track moves in at least runtime or compile time.
    val myOtherVal = myVal

    // FIXME: Due to explained reason, it will evaluate fine.
    intercept[IllegalStateException] {
      immutRef.read(_ => ())
    }
  }
}

class ListShouldNotWorkSuite extends munit.FunSuite {

  test("should not be able to push while peeking immutably") {
    given imem.resource.Context = new imem.resource.DefaultContext

    val list = imem.resource.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)

    val res = imem.peek(list.borrowImmut)

    intercept[IllegalStateException] {
      imem.push(list.borrowMut, 2)
      res.get.read(_ => ()) // idle read
    }
  }

  test("should not be able to push while peeking mutably") {
    given imem.resource.Context = new imem.resource.DefaultContext

    val list = imem.resource.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)

    val res = imem.peekMut(list.borrowMut)

    intercept[IllegalStateException] {
      imem.push(list.borrowMut, 2)
      res.get.read(_ => ()) // idle read
    }
  }

  test("should not be able to push/pop while iterating") {
    given imem.resource.Context = new imem.resource.DefaultContext

    val list = imem.resource.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    val iter = imem.iter(list.borrowImmut)
    intercept[IllegalStateException] {
      imem.pop(list.borrowMut)
      iter.next()
    }
  }

  test("should not be able to peek list after it is consumed") {
    given imem.resource.Context = new imem.resource.DefaultContext

    val list = imem.resource.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    val iter = imem.intoIter(list)

    intercept[IllegalStateException] {
      imem.peek(list.borrowImmut)
    }
  }

  test("should not be able to re-consume list after it is consumed") {
    given imem.resource.Context = new imem.resource.DefaultContext

    val list = imem.resource.Box[imem.List[Int]](imem.List[Int]())
    imem.push(list.borrowMut, 1)
    imem.push(list.borrowMut, 2)
    imem.push(list.borrowMut, 3)

    val iter = imem.intoIter(list)

    intercept[IllegalStateException] {
      imem.intoIter(list)
    }
  }
}
