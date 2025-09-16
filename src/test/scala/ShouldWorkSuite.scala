import language.experimental.captureChecking

class ResourceShouldWorkSuite extends munit.FunSuite:
  test("basics: borrow, mutate and read") {
    imem.withOwnership: ctx =>

      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val myVal = imem.Box.newFromBackground(BoxedInteger(42))(using ctx)

      assert(myVal.borrowImmut(using ctx).read(_ == BoxedInteger(42))(using ctx))
      myVal.borrowMut(using ctx).write(_.value = 12)(using ctx)
      assert(myVal.borrowImmut(using ctx).read(_ == BoxedInteger(12))(using ctx))
  }
end ResourceShouldWorkSuite

class ListShouldWorkSuite extends munit.FunSuite:

  test("basics: push and pop") {
    imem.withOwnership: ctx =>

      val list = imem.Box.newFromBackground(LinkedList.newFromBackground[Int](using ctx))(using ctx)

      val mutList = list.borrowMut[{ctx}, {ctx}](using ctx)

      // Check empty list behaves right
      assertEquals(pop[Int, {ctx}, {ctx}, {ctx}](mutList)(using ctx), None)

      // Populate list
      push(mutList, 1)(using ctx)
      push(mutList, 2)(using ctx)
      push(mutList, 3)(using ctx)

      // // Check normal removal
      assert(pop(mutList)(using ctx).map(_.borrowImmut(using ctx).read(_ == 3)(using ctx)).getOrElse(false))
      assert(pop(mutList)(using ctx).map(_.borrowImmut(using ctx).read(_ == 2)(using ctx)).getOrElse(false))

      // // Push some more just to make sure nothing's corrupted
      push(mutList, 4)(using ctx)
      push(mutList, 5)(using ctx)

      // // Check normal removal
      assert(pop(mutList)(using ctx).map(_.borrowImmut(using ctx).read(_ == 5)(using ctx)).getOrElse(false))
      assert(pop(mutList)(using ctx).map(_.borrowImmut(using ctx).read(_ == 4)(using ctx)).getOrElse(false))

      // // Check exhaustion
      assert(pop(mutList)(using ctx).map(_.borrowImmut(using ctx).read(_ == 1)(using ctx)).getOrElse(false))
      assert(pop(mutList)(using ctx).isEmpty)
  }

  test("peek and peekMut") {
    imem.withOwnership: ctx =>

      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val list = imem.Box.newFromBackground(LinkedList.newFromBackground[BoxedInteger](using ctx))(using ctx)
      assertEquals(peek[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowImmut[{ctx}, {ctx}](using ctx))(using ctx), None)
      assertEquals(peekMut[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowMut(using ctx))(using ctx), None)

      push(list.borrowMut(using ctx), BoxedInteger(1))(using ctx)
      push(list.borrowMut(using ctx), BoxedInteger(2))(using ctx)
      push(list.borrowMut(using ctx), BoxedInteger(3))(using ctx)

      assert(peek[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowImmut(using ctx))(using ctx).map(_.read(_ == BoxedInteger(3))(using ctx)).getOrElse(false))

      // Modify the value using the mutable reference from peekMut

      peekMut[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowMut(using ctx))(using ctx).foreach(ref => ref.write(_.value = 42)(using ctx))

      assert(peek[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowImmut(using ctx))(using ctx).map(_.read(_ == BoxedInteger(42))(using ctx)).getOrElse(false))
      assert(pop(list.borrowMut(using ctx))(using ctx).map(_.borrowImmut(using ctx).read(_ == BoxedInteger(42))(using ctx)).getOrElse(false))
      assert(pop(list.borrowMut(using ctx))(using ctx).map(_.borrowImmut(using ctx).read(_ == BoxedInteger(2))(using ctx)).getOrElse(false))
  }

  test("into_iter: consuming iterator") {
    imem.withOwnership: ctx =>

      val list = imem.Box.newFromBackground(LinkedList.newFromBackground[Int](using ctx))(using ctx)
      push(list.borrowMut(using ctx), 1)(using ctx)
      push(list.borrowMut(using ctx), 2)(using ctx)
      push(list.borrowMut(using ctx), 3)(using ctx)

      val iter = intoIter(list)(using ctx)
      assert(iter.next().borrowImmut(using ctx).read(_ == 3)(using ctx))
      assert(iter.next().borrowImmut(using ctx).read(_ == 2)(using ctx))
      assert(iter.next().borrowImmut(using ctx).read(_ == 1)(using ctx))
      assert(!iter.hasNext)
    }

  test("iter: non-consuming immutable iterator") {
    imem.withOwnership: ctx =>

      val list = imem.Box.newFromBackground(LinkedList.newFromBackground[Int](using ctx))(using ctx)
      push(list.borrowMut(using ctx), 1)(using ctx)
      push(list.borrowMut(using ctx), 2)(using ctx)
      push(list.borrowMut(using ctx), 3)(using ctx)

      val iter = iterImmut[Int, {ctx}, {ctx}, {ctx}](list.borrowImmut(using ctx))(using ctx)
      assert(iter.next().read(_ == 3)(using ctx))
      assert(iter.next().read(_ == 2)(using ctx))

      // Check that the original list is unchanged in the process
      assert(peek[Int, {ctx}, {ctx}, {ctx}](list.borrowImmut(using ctx))(using ctx).map(_.read(_ == 3)(using ctx)).getOrElse(false))

      assert(iter.next().read(_ == 1)(using ctx))
      assert(!iter.hasNext)

      // Check that the original list is unchanged
      assert(peek[Int, {ctx}, {ctx}, {ctx}](list.borrowImmut(using ctx))(using ctx).map(_.read(_ == 3)(using ctx)).getOrElse(false))
  }

  test("iter_mut: non-consuming mutable iterator") {
    imem.withOwnership: ctx =>
      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val list = imem.Box.newFromBackground(LinkedList.newFromBackground[BoxedInteger](using ctx))(using ctx)

      push(list.borrowMut(using ctx), BoxedInteger(1))(using ctx)
      push(list.borrowMut(using ctx), BoxedInteger(2))(using ctx)
      push(list.borrowMut(using ctx), BoxedInteger(3))(using ctx)

      // Use the mutable iterator to modify the elements in the list
      // NOTE: The reason that it's a manual `while` and not `foreach` is that we are using `imem.Iterator` here
      // not `Iterator`.
      val iter = iterMut[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowMut(using ctx))(using ctx)
      while (iter.hasNext) {
        val elemRef = iter.next()
        elemRef.write(elem => elem.value = elem.value * 10)(using ctx)
      }

      // Check that the list contains the new, modified values
      assert(pop(list.borrowMut(using ctx))(using ctx).map(_.borrowImmut(using ctx).read(_ == BoxedInteger(30))(using ctx)).getOrElse(false))
      assert(pop(list.borrowMut(using ctx))(using ctx).map(_.borrowImmut(using ctx).read(_ == BoxedInteger(20))(using ctx)).getOrElse(false))
      assert(pop(list.borrowMut(using ctx))(using ctx).map(_.borrowImmut(using ctx).read(_ == BoxedInteger(10))(using ctx)).getOrElse(false))
      assert(pop(list.borrowMut(using ctx))(using ctx).isEmpty)
  }
end ListShouldWorkSuite
