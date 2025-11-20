import language.experimental.captureChecking

class ResourceShouldWorkSuite extends munit.FunSuite:
  test("basics: borrow, mutate and read") {
    imem.withOwnership: ctx =>
      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val lf = new imem.Lifetime[{ctx}]()
      val valueBox = imem.newBoxExplicit[BoxedInteger, lf.Owners](BoxedInteger(42))

      val (nodeBox2, res) = imem.readBox[BoxedInteger, lf.Owners, Boolean, {ctx}](valueBox, _ == BoxedInteger(42))(using ctx)
      assert(res)
      val (nodeBox3, _) = imem.writeBox[BoxedInteger, lf.Owners, Unit, {ctx}](nodeBox2, _.value = 12)(using ctx)
      val (nodeBox4, res2) = imem.readBox[BoxedInteger, lf.Owners, Boolean, {ctx}](nodeBox3, _ == BoxedInteger(12))(using ctx)
      nodeBox4
      lf
      assert(res2)
  }
end ResourceShouldWorkSuite

class ListShouldWorkSuite extends munit.FunSuite:

  test("basics: push and pop") {
    imem.withOwnership: ctx =>

      val lf = imem.Lifetime[{ctx}]()
      val list = imem.newBoxExplicit[LinkedList[Int, lf.Owners], lf.Owners](newLinkedListExplicit[Int, lf.Owners])

      // Check empty list behaves right
      val list2 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[Int, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](list)(using ctx)
        assertEquals(pop[Int, lf.Owners, InnerLf.Owners, {ctx}, InnerLf.Owners](listRef)(using ctx).isEmpty, true)
        imem.accessValue(InnerLf.getKey(), listHolder)

      // Populate list
      val list3 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[Int, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](list2)(using ctx)
        push(listRef, 1)(using ctx)
        imem.accessValue(InnerLf.getKey(), listHolder)

      val list4 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[Int, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](list3)(using ctx)
        push(listRef, 2)(using ctx)
        imem.accessValue(InnerLf.getKey(), listHolder)

      // Check normal removal
      val list5 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[Int, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](list4)(using ctx)
        val poppedBox = pop[Int, lf.Owners, InnerLf.Owners, {ctx}, InnerLf.Owners](listRef)(using ctx).get
        assert(imem.readBox(poppedBox, _ == 2)(using ctx)._2)
        imem.accessValue(InnerLf.getKey(), listHolder)

      // Push some more just to make sure nothing's corrupted
      val list6 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[Int, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](list5)(using ctx)
        push(listRef, 3)(using ctx)
        imem.accessValue(InnerLf.getKey(), listHolder)

      // Check normal removal
      val list7 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[Int, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](list6)(using ctx)
        val poppedBox = pop[Int, lf.Owners, InnerLf.Owners, {ctx}, InnerLf.Owners](listRef)(using ctx).get
        assert(imem.readBox(poppedBox, _ == 3)(using ctx)._2)
        imem.accessValue(InnerLf.getKey(), listHolder)

      // Check exhaustion
      val list8 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[Int, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](list7)(using ctx)
        val poppedBox = pop[Int, lf.Owners, InnerLf.Owners, {ctx}, InnerLf.Owners](listRef)(using ctx).get
        assert(imem.readBox(poppedBox, _ == 1)(using ctx)._2)


        imem.accessValue(InnerLf.getKey(), listHolder)
      val list9 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[Int, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](list8)(using ctx)
        assertEquals(pop[Int, lf.Owners, InnerLf.Owners, {ctx}, InnerLf.Owners](listRef)(using ctx).isEmpty, true)
        imem.accessValue(InnerLf.getKey(), listHolder)

      list9
      lf
      ()
  }

  test("peek and peekMut") {
    imem.withOwnership: ctx =>

      // A way to express a mutable integer.
      case class BoxedInteger(var value: Int)
      val list = imem.newBoxFromBackground(newLinkedListFromBackground[BoxedInteger](using ctx))(using ctx)

      val list2 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowImmutBox[LinkedList[BoxedInteger, {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners](list)(using ctx)
        assertEquals(peek[BoxedInteger, {ctx}, InnerLf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](listRef)(using ctx).isEmpty, true)
        imem.accessValue(InnerLf.getKey(), listHolder)

      val list3 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[BoxedInteger, {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners](list2)(using ctx)
        assertEquals(peekMut[BoxedInteger, {ctx}, InnerLf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](listRef)(using ctx).isEmpty, true)
        imem.accessValue(InnerLf.getKey(), listHolder)

      val list4 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[BoxedInteger, {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners](list3)(using ctx)
        push(listRef, BoxedInteger(1))(using ctx)
        imem.accessValue(InnerLf.getKey(), listHolder)

      val list5 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowImmutBox[LinkedList[BoxedInteger, {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners](list4)(using ctx)
        val peekedBox = peek[BoxedInteger, {ctx}, InnerLf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](listRef)(using ctx).get
        assertEquals(imem.read[BoxedInteger, InnerLf.Owners, Boolean, {ctx}](peekedBox, _.value == 1)(using ctx), true)
        imem.accessValue(InnerLf.getKey(), listHolder)

      // Modify the value using the mutable reference from peekMut

      val list6 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowMutBox[LinkedList[BoxedInteger, {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners](list5)(using ctx)
        val peekedMut = peekMut[BoxedInteger, {ctx}, InnerLf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](listRef)(using ctx).get
        imem.write[BoxedInteger, InnerLf.Owners, Unit, {ctx}](peekedMut, _.value = 42)(using ctx)
        imem.accessValue(InnerLf.getKey(), listHolder)

      val list7 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowImmutBox[LinkedList[BoxedInteger, {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners](list6)(using ctx)
        val peeked = peek[BoxedInteger, {ctx}, InnerLf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners](listRef)(using ctx).get
        assertEquals(imem.read[BoxedInteger, InnerLf.Owners, Boolean, {ctx}](peeked, _.value == 42)(using ctx), true)
        imem.accessValue(InnerLf.getKey(), listHolder)

      list7
      ()
  }
end ListShouldWorkSuite
