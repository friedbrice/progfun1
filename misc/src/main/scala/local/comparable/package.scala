package local

package object comparable {

  trait Comparable[-A] {
    def isLess(x: A, y: A): Boolean
    def isMore(x: A, y: A): Boolean
  }

  def isLess[A: Comparable](x: A, y: A): Boolean =
    implicitly[Comparable[A]].isLess(x, y)

  def isMore[A: Comparable](x: A, y: A): Boolean =
    implicitly[Comparable[A]].isMore(x, y)

  def isSame[A: Comparable](x: A, y: A): Boolean =
    !isLess(x, y) && !isMore(x, y)

  def smaller[A: Comparable](x: A, y: A): A =
    if (isLess(x, y)) x else y

  def bigger[A: Comparable](x: A, y: A): A =
    if (isMore(x, y)) x else y

  def smallest[A: Comparable](xs: Seq[A]): A =
    xs.reduce(smaller(_, _))

  def biggest[A: Comparable](xs: Seq[A]): A =
    xs.reduce(bigger(_, _))

  object Comparable {

    implicit object ComparableInt extends Comparable[Int] {
      def isLess(x: Int, y: Int) = x < y
      def isMore(x: Int, y: Int) = x > y
    }

    implicit object ComparableDouble extends Comparable[Double] {
      def isLess(x: Double, y: Double) = x < y
      def isMore(x: Double, y: Double) = x > y
    }
  }
}
