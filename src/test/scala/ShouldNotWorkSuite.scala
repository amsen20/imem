import language.experimental.captureChecking
import imem.*
import scinear.utils.LinearInt

// omitting any of `O1`, or `O2` would result in a compile error:
//                                                 v   v
def longest[O4^, O1^ >: {O4}, O2^ >: {O4}, O3^ >: {O1, O2}, WC^, MC^](
  a: ImmutRef[String, O1],
  b: ImmutRef[String, O2]
)(using ctx: Context[WC, MC]^{O4}): ImmutRef[String, O3] =
  val isALessEqualB = read[String, O1, Boolean, O4, WC, MC](a, aVal =>
    read[String, O2, Boolean, O1, WC, MC](b, bVal =>
      aVal.length <= bVal.length
    )
  )
  if isALessEqualB then
    // if `O1` is omitted here, it would result in a compile error:
    borrowImmut[String, O1, O4, NeverUsableKey, O3, WC, MC](a)
    //                                          ^
    // Type argument O3 does not conform to lower bound scala.caps.CapSet^{O1}
  else
    // if `O2` is omitted here, it would result in a compile error:
    borrowImmut[String, O2, O4, NeverUsableKey, O3, WC, MC](b)
    //                                          ^
    // Type argument O3 does not conform to lower bound scala.caps.CapSet^{O2}

def IteratorsOnList[O1^, O2^ >: {O1}, @caps.use WC^, @caps.use MC^]()(using ctx: Context[WC, MC]^{O2}): Unit =
  // create an empty list with {ctx} as its lifetime
  val list = newBoxFromBackground(newLinkedListFromBackground[LinearInt, {WC}, {MC}])

  // a lifetime for the mutable iterator
  val lf1 = Lifetime[{ctx}]()

  // borrow the list mutably to use it for the mutable iterator
  val (listRef, listHolder) = imem.borrowMutBox[List[LinearInt, {ctx}], {ctx}, {ctx}, lf1.Key, lf1.Owners, {WC}, {MC}](list)
  // create the mutable iterator
  val mutIter = iterMut[LinearInt, {ctx}, lf1.Owners, lf1.Owners, {WC}, {MC}](listRef)
  // don't need `iter` anymore, so consume it
  mutIter.consume()

  // get the list back
  val list2 = unlockHolder(lf1.getKey(), listHolder)

  // create a consuming iterator from the list
  val consumingIter = intoIter[LinearInt, {ctx}, {ctx}, {WC}, {MC}](list2)
  // wrap it in a box
  val iterBox = newBox[ConsumingIterator[LinearInt, {ctx}], {ctx}](consumingIter)
  // don't need `iterBox` anymore, so consume it
  iterBox.consume()

  ()
