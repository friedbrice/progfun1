package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c + 1).to(r).product / 1.to(r-c).product

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    chars
      .filter(c => c == '(' || c == ')')
      .foldLeft(List[Char]())(
        (acc, c) => c match {
          case '(' => '(' :: acc
          case ')' => scala.util.Try(acc.tail).getOrElse(chars)
        }
      )
      .isEmpty

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    val cs = coins.toSet
    val m = money
    val lists = scala.collection.mutable.HashMap[Int, Set[List[Int]]](
      0 -> Set[List[Int]](),
      1 -> cs.map(List(_))
    )

    def getLists(k: Int): Set[List[Int]] = lists.getOrElse(k, genLists(k))

    def genLists(k: Int): Set[List[Int]] = {
      val newList = getLists(k - 1)
        .flatMap(l => cs.map(c => (c :: l).sorted))
        .filter(_.sum <= m)
      lists.update(k, newList)
      newList
    }

    1.to(m).toSet
      .flatMap(getLists)
      .count(_.sum == m)
  }

}
