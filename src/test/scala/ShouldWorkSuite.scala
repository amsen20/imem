import language.experimental.captureChecking
import scinear.utils.LinearInt
import imem.*

class BoxedInteger[O^](_value: imem.Box[Int, O]) extends scinear.Linear:
  val value: imem.Box[Int, O]^{this} = _value
end BoxedInteger

class ResourceShouldWorkSuite extends munit.FunSuite:
  test("basics: borrow, mutate and read") {
    def body[@caps.use WC^, @caps.use MC^](using ctx: imem.Context[WC, MC]^): Unit =
      val lf = new imem.Lifetime[{ctx}]()
      val valueBox = imem.newBox[Int, lf.Owners](42)

      val (nodeBox2, res) = imem.readBox[Int, lf.Owners, Boolean, {ctx}, WC, MC](valueBox, _ == 42)
      assert(res)
      val nodeBox3 = imem.setBox(nodeBox2, 12)

      val (nodeBox4, res2) = imem.readBox[Int, lf.Owners, Boolean, {ctx}, WC, MC](nodeBox3, _ == 12)
      assert(res2)

      nodeBox4.consume()
      lf.consume()

    imem.withImem([WC^, MC^] => ctx => body[WC, MC](using ctx))
  }
end ResourceShouldWorkSuite

class ListShouldWorkSuite extends munit.FunSuite:

  test("basics: push and pop") {
    def body[@caps.use WC^, @caps.use MC^](using ctx: imem.Context[WC, MC]^): Unit =
      val lf = imem.Lifetime[{ctx}]()
      val list = imem.newBox[List[LinearInt, lf.Owners], lf.Owners](newLinkedListExplicit[LinearInt, lf.Owners])

      // check how an empty list behaves right:
      val list2 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[LinearInt, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](list)
        assertEquals(pop(listRef).isEmpty, true)
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      // populate the list:
      val list3 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[LinearInt, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](list2)
        push(listRef, LinearInt(1))
        // list: [1]
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      val list4 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[LinearInt, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](list3)
        push(listRef, LinearInt(2))
        // list: [2, 1]
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      // check popping the first element:
      val list5 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[LinearInt, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](list4)
        val poppedBox = pop[LinearInt, lf.Owners, InnerLf.Owners, InnerLf.Owners, {WC}, {MC}](listRef).get
        // list: [1]
        assert(imem.readBox[LinearInt, InnerLf.Owners, Boolean, {ctx}, {WC}, {MC}](poppedBox, _.value == 2)._2)
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      // push some more just to make sure nothing's corrupted:
      val list6 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[LinearInt, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](list5)
        push(listRef, LinearInt(3))
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      // Check normal removal
      val list7 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[LinearInt, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](list6)
        val poppedBox = pop[LinearInt, lf.Owners, InnerLf.Owners, InnerLf.Owners, {WC}, {MC}](listRef).get
        assert(imem.readBox[LinearInt, InnerLf.Owners, Boolean, {ctx}, {WC}, {MC}](poppedBox, _.value == 3)._2)
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      // Check exhaustion
      val list8 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[LinearInt, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](list7)
        val poppedBox = pop[LinearInt, lf.Owners, InnerLf.Owners, InnerLf.Owners, {WC}, {MC}](listRef).get
        assert(imem.readBox[LinearInt, InnerLf.Owners, Boolean, {ctx}, {WC}, {MC}](poppedBox, _.value == 1)._2)


        imem.unlockHolder(InnerLf.getKey(), listHolder)
      val list9 =
        val InnerLf = new imem.Lifetime[{ctx, lf}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[LinearInt, lf.Owners], lf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](list8)
        assertEquals(pop(listRef).isEmpty, true)
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      list9.consume()
      lf.consume()
      ()
    imem.withImem([WC^, MC^] => ctx => body[WC, MC](using ctx))
  }

  test("peek and peekMut and iterMut and iterConsume") {
    def body[@caps.use WC^, @caps.use MC^](using ctx: imem.Context[WC, MC]^): Unit =
      val list = imem.newBoxFromBackground(newLinkedListFromBackground[BoxedInteger[{ctx}], WC, MC](using ctx))(using ctx)

      val list2 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowImmutBox[List[BoxedInteger[{ctx}], {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners, WC, MC](list)
        assertEquals(peek(listRef)(using ctx).isEmpty, true)
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      val list3 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[BoxedInteger[{ctx}], {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, MC](list2)
        assertEquals(peekMut(listRef)(using ctx).isEmpty, true)
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      val list4 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[BoxedInteger[{ctx}], {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, MC](list3)
        push(listRef, BoxedInteger[{ctx}](imem.newBox(1)))(using ctx)
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      val list5 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowImmutBox[List[BoxedInteger[{ctx}], {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners, WC, MC](list4)
        val peeked = peek[BoxedInteger[{ctx}], {ctx}, InnerLf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](listRef)(using ctx).get

        val is1 = imem.read[BoxedInteger[{ctx}], InnerLf.Owners, Boolean, {ctx}, {WC}, {MC}](
          peeked, boxedInt => imem.readBox[Int, {ctx}, Boolean, {ctx, InnerLf}, {WC}, {MC}](boxedInt.value, _ == 1)._2)
        assert(is1)

        imem.unlockHolder(InnerLf.getKey(), listHolder)

      // Modify the value using the mutable reference from peekMut

      val list6 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[BoxedInteger[{ctx}], {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, MC](list5)
        val peekedMut = peekMut[BoxedInteger[{ctx}], {ctx}, InnerLf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](listRef).get
        imem.write[BoxedInteger[{ctx}], InnerLf.Owners, Unit, {ctx}, {WC}, {MC}](peekedMut, (boxedInt: BoxedInteger[{ctx}]^) => imem.setBox[Int, {ctx}, {ctx, InnerLf}, {WC}, {MC}](boxedInt.value, 42))(using ctx)
        imem.unlockHolder(InnerLf.getKey(), listHolder)

      val list7 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowImmutBox[List[BoxedInteger[{ctx}], {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners, WC, MC](list6)
        val peeked = peek[BoxedInteger[{ctx}], {ctx}, InnerLf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](listRef).get

        val is42 = imem.read[BoxedInteger[{ctx}], InnerLf.Owners, Boolean, {ctx}, {WC}, {MC}](peeked, (boxedInt: BoxedInteger[{ctx}]^) => imem.readBox[Int, {ctx}, Boolean, {ctx, InnerLf}, {WC}, {MC}](boxedInt.value, _ == 42)._2)(using ctx)
        assert(is42)

        imem.unlockHolder(InnerLf.getKey(), listHolder)

      val list8 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (listRef, listHolder) = imem.borrowMutBox[List[BoxedInteger[{ctx}], {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, MC](list7)
        val iter = iterMut[BoxedInteger[{ctx}], {ctx}, {ctx}, InnerLf.Owners, {WC}, {MC}](listRef)
        val iterBox = imem.newBox[MutableIterator[BoxedInteger[{ctx}], {ctx}, InnerLf.Owners], InnerLf.Owners](iter)
        val (iterRef, iterHolder) = imem.borrowMutBox[MutableIterator[BoxedInteger[{ctx}], {ctx}, InnerLf.Owners], InnerLf.Owners, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](iterBox)
        iterHolder.consume()
        val firstElemOp2 = nextMI[BoxedInteger[{ctx}], {ctx}, InnerLf.Owners, InnerLf.Owners, {WC}, {MC}](iterRef)

        val (firElemOpt2, isEmpty) = scinear.utils.peekLinearOption(firstElemOp2)
        assert(!isEmpty)
        val firstElem2 = firElemOpt2.get
        val is42 = imem.write[BoxedInteger[{ctx}], InnerLf.Owners, Boolean, {ctx}, {WC}, {MC}](firstElem2, (boxedInt: BoxedInteger[{ctx}]^) => imem.readBox[Int, {ctx}, Boolean, {ctx, InnerLf}, {WC}, {MC}](boxedInt.value, _ == 42)._2)
        assert(is42)

        imem.unlockHolder(InnerLf.getKey(), listHolder)

      val iter = intoIter[BoxedInteger[{ctx}], {ctx}, {ctx}, {WC}, {MC}](list8)
      val iterBox = imem.newBox[ConsumingIterator[BoxedInteger[{ctx}], {ctx}], {ctx}](iter)

      val iterBox2 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (iterRef, iterHolder) = imem.borrowImmutBox[ConsumingIterator[BoxedInteger[{ctx}], {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](iterBox)
        val hasNext = hasNextCI[BoxedInteger[{ctx}], {ctx}, InnerLf.Owners, {WC}, {MC}](iterRef)
        assert(hasNext)
        imem.unlockHolder(InnerLf.getKey(), iterHolder)

      val iterBox3 =
        val InnerLf = new imem.Lifetime[{ctx}]()
        val (iterRef, iterHolder) = imem.borrowMutBox[ConsumingIterator[BoxedInteger[{ctx}], {ctx}], {ctx}, {ctx}, InnerLf.Key, InnerLf.Owners, {WC}, {MC}](iterBox2)
        val firstElemOpt = nextCI[BoxedInteger[{ctx}], {ctx}, InnerLf.Owners, {WC}, {MC}](iterRef)

        val (firstElemOpt2, isEmpty) = scinear.utils.peekLinearOption(firstElemOpt)
        assert(!isEmpty)
        val firstElem = firstElemOpt2.get
        val (firstElemImmutRef, firstElemImmutHolder) = borrowImmutBox[BoxedInteger[{ctx}], InnerLf.Owners, {ctx}, imem.NeverUsableKey, InnerLf.Owners, {WC}, {MC}](firstElem)
        firstElemImmutHolder.consume()
        val is42 = imem.read[BoxedInteger[{ctx}], InnerLf.Owners, Boolean, {ctx}, {WC}, {MC}](firstElemImmutRef, (boxedInt: BoxedInteger[{ctx}]^) => imem.readBox[Int, {ctx}, Boolean, {ctx, InnerLf}, {WC}, {MC}](boxedInt.value, _ == 42)._2)
        assert(is42)

        imem.unlockHolder(InnerLf.getKey(), iterHolder)

      iterBox3.consume()
      ()
    imem.withImem([WC^, MC^] => ctx => body[WC, MC](using ctx))
  }
end ListShouldWorkSuite
