package object patmat {

  sealed trait CodeTree

  case class Fork(
    left: CodeTree,
    right: CodeTree,
    chars: List[Char],
    weight: Int
  ) extends CodeTree

  case class Leaf(
    char: Char,
    weight: Int
  ) extends CodeTree

  def weight(tree: CodeTree): Int = tree match {
    case Fork(_, _, _, w) => w
    case Leaf(_, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(_, _, cs, _) => cs
    case Leaf(c, _) => List(c)
  }

  def makeCodeTree(left: CodeTree, right: CodeTree): Fork = Fork(
    left = left,
    right = right,
    chars = chars(left) ::: chars(right),
    weight = weight(left) + weight(right)
  )

  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] =
    chars
      .foldLeft(Map[Char, Int]())(
        (acc, c) => acc.updated(c, acc.getOrElse(c, 0) + 1)
      )
      .toList

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    freqs
      .sortBy({case (c, w) => w})
      .map({case (c, w) => Leaf(c, w)})

  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case _ :: Nil => true
    case _ => false
  }

  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case tree1 :: tree2 :: rest =>
      val combined = makeCodeTree(tree1, tree2)
      val (left, right) = rest.span({
        case Leaf(_, w) => w < combined.weight
        case Fork(_, _, _, w) => w < combined.weight
      })
      left ::: combined :: right
    case _ => trees
  }

  def until[A](p: A => Boolean, f: A => A)(x: A): A =
    if (p(x)) x else until(p, f)(f(x))

  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

    def helper(
      subtree: CodeTree,
      acc: List[Char],
      rest: List[Bit]
    ): List[Char] = subtree match {
      case Leaf(char, _) => helper(tree, char :: acc, rest)
      case Fork(left, right, _, _) => rest match {
        case Nil => acc
        case 0 :: nextRest => helper(left, acc, nextRest)
        case 1 :: nextRest => helper(right, acc, nextRest)
      }
    }
    helper(tree, Nil, bits).reverse
  }

  def charToBits(tree: CodeTree)(char: Char): List[Bit] = {

    def helper(acc: List[Bit], rest: CodeTree): List[Bit] = {

      rest match {
        case Leaf(_, _) => acc
        case Fork(l, r, _, _) => {

          def getChars(tree: CodeTree): List[Char] = tree match {
            case Fork(_, _, cs, _) => cs
            case Leaf(c, _) => List(c)
          }

          if (getChars(l).contains(char))
            helper(0 :: acc, l)
          else if (getChars(r).contains(char))
            helper(1 :: acc, r)
          else
            Nil
        }
      }
    }

    helper(List(), tree).reverse
  }

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
    text.flatMap(charToBits(tree))

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] =
    table.toMap.apply(char)

  def convert(tree: CodeTree): CodeTable = {

    def helper(acc: CodeTable, rest: CodeTree): CodeTable = rest match {
      case Leaf(c, _) => (c, charToBits(tree)(c)) :: acc
      case Fork(l, r, _, _) => helper(acc, l) ++ helper(acc, r)
    }

    helper(List(), tree)
  }

  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = a ++ b

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] =
    text.flatMap(codeBits(convert(tree)))
}
