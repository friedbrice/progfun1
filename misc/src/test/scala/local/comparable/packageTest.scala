package local.comparable

import org.scalatest.{FlatSpec, Matchers}

class packageTest extends FlatSpec with Matchers {

  "ComperableInt" should "be implicitly accessible" in {
    // given
    val x: Int = 4
    val y: Int = 7

    // when
    val result1 = isLess(4, 7)
    val result2 = isMore(4, 7)
    val result3 = isSame(4, 7)

    // then
    result1 should be(4 < 7)
    result2 should be(4 > 7)
    result3 should be(4 == 7)
  }

  "ComperableDouble" should "be implicitly accessible" in {
    // given
    val x: Double = 4
    val y: Double = 7

    // when
    val result1 = isLess(4, 7)
    val result2 = isMore(4, 7)
    val result3 = isSame(4, 7)

    // then
    result1 should be(4 < 7)
    result2 should be(4 > 7)
    result3 should be(4 == 7)
  }

  "Comparable" should "be open to future implementation" in {
    // given
    sealed trait SimpleOrder
    case object Smallest extends SimpleOrder
    case object Biggest extends SimpleOrder

    // when
    implicit object ComparableSimpleOrder extends Comparable[SimpleOrder] {

      def isLess(x: SimpleOrder, y: SimpleOrder): Boolean = (x, y) match {
        case (Smallest, Biggest) => true
        case _ => false
      }

      def isMore(x: SimpleOrder, y: SimpleOrder): Boolean = (x, y) match {
        case (Biggest, Smallest) => true
        case _ => false
      }
    }

    // then
    isLess[SimpleOrder](Smallest, Smallest) should be(false)
    isLess[SimpleOrder](Smallest, Biggest) should be (true)
    isLess[SimpleOrder](Biggest, Smallest) should be (false)
    isLess[SimpleOrder](Biggest, Biggest) should be (false)

    isMore(Smallest, Smallest) should be(false)
    isMore(Smallest, Biggest) should be(false)
    isMore(Biggest, Smallest) should be(true)
    isMore(Biggest, Biggest) should be(false)

    isSame(Smallest, Smallest) should be(true)
    isSame(Smallest, Biggest) should be(false)
    isSame(Biggest, Smallest) should be(false)
    isSame(Biggest, Biggest) should be(true)
  }
}
