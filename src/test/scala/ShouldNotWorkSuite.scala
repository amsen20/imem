import language.experimental.captureChecking

class ResourceShouldNotWorkSuite extends munit.FunSuite {
  test("should not be able to call the Box default constructor") {
    imem.withOwnership: ctx =>
      // TODO: Should not compile, should make the constructor private.
      // FIXME: Due to explained reason, it will evaluate fine.
      intercept[Exception] {
        new imem.Box()
      }
  }

  test("box should not be able assigned to a variable") {
    imem.withOwnership: ctx =>
      // TODO: A `Box` type should not be able to be assigned to a variable because it forces the user to use `Box`'s
      // methods to set and reset it. Using them we can keep track of the ownership state.
      // FIXME: Due to explained reason, it will evaluate fine.
      intercept[Exception] {
        var myVal = imem.newBoxExplicit[Int, {}](42)
      }
  }

  test("should not be able to write to a reference, by reading it") {
    imem.withOwnership: ctx =>
      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val myVal = imem.newBoxFromBackground(BoxedInteger(42))(using ctx)

      // TODO: For now, there is no difference between, `read` and `write` methods. Both can mutate the value. This
      // Should be changed, at least in runtime or compile-time, mutation through `read`s should not be allowed.
      // FIXME: Due to explained reason, it will evaluate fine.
      intercept[IllegalStateException] {
        imem.readBox(myVal, v => v.value = 12)(using ctx)
      }
  }

  test("should not be able to escape a value through `read`/`write` methods of a reference") {
    imem.withOwnership: ctx =>
      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val myVal = imem.newBoxFromBackground(BoxedInteger(42))(using ctx)

      // TODO: For now, values can be leaked though `read` and `write` methods. This should be changed, at least in
      // runtime or compile-time.
      // FIXME: Due to explained reason, it will evaluate fine.
      intercept[Exception] {
        imem.readBox(myVal, v => v)(using ctx).value
      }
  }

  test("should not be able to mutate, while reading it") {
    imem.withOwnership: ctx =>
      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val myVal = imem.newBoxFromBackground(BoxedInteger(42))(using ctx)

      val immutRef = imem.borrowImmutBox[BoxedInteger, {ctx}, {ctx}, {ctx}](myVal)(using ctx)
      imem.writeBox(myVal, _.value = 12)(using ctx)
      intercept[IllegalStateException] {
        imem.read(immutRef, _ => ())(using ctx)
      }
  }

  test("borrows should be invalidated after moving") {
    imem.withOwnership: ctx =>
      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val myVal = imem.newBoxFromBackground(BoxedInteger(42))(using ctx)
      val immutRef = imem.borrowImmutBox[BoxedInteger, {ctx}, {ctx}, {ctx}](myVal)(using ctx)

      // TODO: For now no moving runtime/compile time effect is defined, so the following line will have no effect in the
      // stacked borrows. Should track moves in at least runtime or compile time.
      val myOtherVal = myVal

      // FIXME: Due to explained reason, it will evaluate fine.
      intercept[IllegalStateException] {
        imem.read(immutRef, _ => ())(using ctx)
      }
  }

  test(
    "should not be able to go around a reference connection with its owner by wrapping it up in another reference"
  ) {
    imem.withOwnership: ctx =>
      val main = imem.newBoxFromBackground(imem.newBoxFromBackground(1)(using ctx))(using ctx)
      val dummy = imem.newBoxFromBackground(2)(using ctx)

      val ref1 = imem.borrowMutBox[imem.Box[Int, {ctx}]^{ctx}, {ctx}, {ctx}, {ctx}](main)(using ctx)
      val dummyRef = imem.borrowImmutBox[Int, {ctx}, {ctx}, {ctx}](dummy)(using ctx)

      val ref2 = imem.write[imem.Box[Int, {ctx}]^{ctx}, {ctx}, imem.ImmutRef[Int, {ctx}]^{ctx}, {ctx}](
        ref1,
        (inner: imem.Box[Int, {ctx}]^{ctx}) =>
          imem.read(dummyRef, _ => imem.borrowImmutBox[Int, {ctx}, {ctx}, {ctx}](inner)(using ctx))
      )(using ctx) /* (inner => dummyRef.read(_ => (inner.borrowImmut)))*/

      imem.write(ref1, _ => ())(using ctx) // should expire `ref2`
      intercept[IllegalStateException] {
        imem.read(ref2, _ => ())(using ctx)
      }
      imem.read(dummyRef, _ => ())(using ctx)
  }

  test(
    "should invalidate every reference from a box after the box is out of scope"
  ) {
    imem.withOwnership: ctx =>
      // FIXME: should not be able to compile this
      // TODO: For now, I don't know how to define it, so I comment it.
      // def leakImmutRef(b: imem.Box[Int]^) =
      // b.borrowImmut
      // NOTE: For now, intentionally fail this test.
      assert(false)
  }

  test("should invalidate a box owned by another box after the box is out of scope") {
    imem.withOwnership: ctx =>
      // FIXME: should not be able to compile this
      // TODO: For now, I don't know how to define it, so I comment it.
      // def leakMutRef(outer: imem.Box[imem.Box[Int]]^) =
      // outer.borrowImmut.read(identity)
      // NOTE: For now, intentionally fail this test.
      assert(false)
  }

  test("should not be able to access self owned boxes in any way other than their holders") {
    imem.withOwnership: ctx =>
      val orig: imem.OwnerOrigin^ = new imem.OwnerOrigin
      val orig2: imem.OwnerOrigin^ = new imem.OwnerOrigin
      val listHolder = new imem.BoxHolder[orig.Key, imem.Box[Int, {orig}]^{orig}, {orig}](imem.newBoxExplicit[imem.Box[Int, {orig}]^{orig}, {orig}](imem.newBoxExplicit[Int, {orig}](42)))
      // OK
      val list = listHolder.getBox(orig.getKey())

      /*
      * FIXME: For now compile errors, cannot be tested in this unit, can be fixed by writing
      * tests like the compiler plugin tests.
      */
      // val list2 = listHolder.getBox(orig2.getKey())
      // val list3 = listHolder.getBox(new Object)
  }
}

class ListShouldNotWorkSuite extends munit.FunSuite {

  test("should not be able to push while peeking immutably") {
    imem.withOwnership: ctx =>
      val list = imem.newBoxFromBackground(newLinkedListFromBackground[Int](using ctx))(using ctx)
      push(imem.borrowMutBox(list)(using ctx), 1)(using ctx)

      val res = peek[Int, {ctx}, {ctx}, {ctx}, {ctx}](imem.borrowImmutBox(list)(using ctx))(using ctx)

      intercept[IllegalStateException] {
        push(imem.borrowMutBox(list)(using ctx), 2)(using ctx)
        imem.read(res.get, _ => ())(using ctx) // idle read
      }
      ()
  }

  test("should not be able to push while peeking mutably") {
    imem.withOwnership: ctx =>
      val list = imem.newBoxFromBackground(newLinkedListFromBackground[Int](using ctx))(using ctx)
      push(imem.borrowMutBox(list)(using ctx), 1)(using ctx)

      val res = peekMut[Int, {ctx}, {ctx}, {ctx}, {ctx}](imem.borrowMutBox(list)(using ctx))(using ctx)

      intercept[IllegalStateException] {
        push(imem.borrowMutBox(list)(using ctx), 2)(using ctx)
        imem.write(res.get, _ => ())(using ctx) // idle read
      }
  }
}