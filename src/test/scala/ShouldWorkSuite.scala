import language.experimental.captureChecking

class ResourceShouldWorkSuite extends munit.FunSuite:
  test("basics: borrow, mutate and read") {
    imem.withOwnership: ctx =>

      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val orig: imem.OwnerOrigin^ = new imem.OwnerOrigin
      val valueHolder = new imem.BoxHolder[orig.Key, BoxedInteger, {orig}](imem.Box.newExplicit[BoxedInteger, {orig}](BoxedInteger(42)))
      val valueBox = valueHolder.getBox(orig.getKey())
      val immutRef = valueBox.borrowImmut[{ctx}, {ctx, orig}](using ctx)

      assert(imem.readBox(valueBox, _ == BoxedInteger(42))(using ctx))
      valueBox.borrowMut[{ctx}, {ctx, orig}](using ctx).write(_.value = 12)(using ctx)
      assert(imem.readBox(valueBox, _ == BoxedInteger(12))(using ctx))
  }
end ResourceShouldWorkSuite

class ListShouldWorkSuite extends munit.FunSuite:

  test("basics: push and pop") {
    imem.withOwnership: ctx =>

      val orig: imem.OwnerOrigin^ = new imem.OwnerOrigin
      val listHolder = new imem.BoxHolder[orig.Key, LinkedList[Int, {orig}]^{orig}, {orig}](imem.Box.newExplicit[LinkedList[Int, {orig}]^{orig}, {orig}](LinkedList.newExplicit[Int, {orig}]))
      val list = listHolder.getBox(orig.getKey())

      val mutList = list.borrowMut[{ctx}, {ctx, orig}](using ctx)

      // Check empty list behaves right
      assertEquals(pop[Int, {orig}, {ctx, orig}, {ctx, orig}](mutList)(using ctx).isEmpty, true)

      // Populate list
      push(mutList, 1)(using ctx)
      push(mutList, 2)(using ctx)
      push(mutList, 3)(using ctx)

      // Check normal removal
      assert(pop[Int, {orig}, {ctx, orig}, {ctx, orig}](mutList)(using ctx).map(imem.readBox(_, _ == 3)(using ctx)).getOrElse(false))
      assert(pop[Int, {orig}, {ctx, orig}, {ctx, orig}](mutList)(using ctx).map(imem.readBox(_, _ == 2)(using ctx)).getOrElse(false))

      // Push some more just to make sure nothing's corrupted
      push[Int, {orig}, {ctx, orig}](mutList, 4)(using ctx)
      push[Int, {orig}, {ctx, orig}](mutList, 5)(using ctx)

      // Check normal removal
      assert(pop[Int, {orig}, {ctx, orig}, {ctx, orig}](mutList)(using ctx).map(imem.readBox(_, _ == 5)(using ctx)).getOrElse(false))
      assert(pop[Int, {orig}, {ctx, orig}, {ctx, orig}](mutList)(using ctx).map(imem.readBox(_, _ == 4)(using ctx)).getOrElse(false))

      // Check exhaustion
      assert(pop[Int, {orig}, {ctx, orig}, {ctx, orig}](mutList)(using ctx).map(imem.readBox(_, _ == 1)(using ctx)).getOrElse(false))
      assert(pop[Int, {orig}, {ctx, orig}, {ctx, orig}](mutList)(using ctx).isEmpty)
  }

  test("peek and peekMut") {
    imem.withOwnership: ctx =>

      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val list = imem.Box.newFromBackground(LinkedList.newFromBackground[BoxedInteger](using ctx))(using ctx)
      assertEquals(peek[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowImmut[{ctx}, {ctx}](using ctx))(using ctx).isEmpty, true)
      assertEquals(peekMut[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowMut[{ctx}, {ctx}](using ctx))(using ctx).isEmpty, true)

      push(list.borrowMut[{ctx}, {ctx}](using ctx), BoxedInteger(1))(using ctx)
      push(list.borrowMut[{ctx}, {ctx}](using ctx), BoxedInteger(2))(using ctx)
      push(list.borrowMut[{ctx}, {ctx}](using ctx), BoxedInteger(3))(using ctx)

      assert(
        peek[BoxedInteger, {ctx}, {ctx}, {ctx}](
            list.borrowImmut[{ctx}, {ctx}](using ctx)
          )(using ctx).map((item: imem.ImmutRef[BoxedInteger, {ctx}]^{ctx}) => imem.read[BoxedInteger, {ctx}, Boolean, {ctx}, BoxedInteger](item, newCtx ?=> data => data == BoxedInteger(3))(using ctx)).getOrElse(false)
      )

      // Modify the value using the mutable reference from peekMut

      peekMut[BoxedInteger, {ctx}, {ctx}, {ctx}](
        list.borrowMut[{ctx}, {ctx}](using ctx)
      )(using ctx).foreach((ref: imem.MutRef[BoxedInteger, {ctx}]^{ctx}) => ref.write(_.value = 42)(using ctx))

      assert(peek[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowImmut(using ctx))(using ctx).map((item: imem.ImmutRef[BoxedInteger, {ctx}]^{ctx}) => imem.read(item, _ == BoxedInteger(42))(using ctx)).getOrElse(false))
      assert(pop[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowMut[{ctx}, {ctx}](using ctx))(using ctx).map(imem.readBox(_, _ == BoxedInteger(42))(using ctx)).getOrElse(false))
      assert(pop[BoxedInteger, {ctx}, {ctx}, {ctx}](list.borrowMut[{ctx}, {ctx}](using ctx))(using ctx).map(imem.readBox(_, _ == BoxedInteger(2))(using ctx)).getOrElse(false))
  }
end ListShouldWorkSuite
