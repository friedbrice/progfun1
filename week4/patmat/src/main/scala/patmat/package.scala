import common._

/**
  * Assignment 4: Huffman coding
  */
package object patmat {

  /**
    * A huffman code is represented by a binary tree.
    *
    * Every `Leaf` node of the tree represents one character of the alphabet
    * that the tree can encode.
    * The weight of a `Leaf` is the frequency of appearance of the character.
    *
    * The branches of the huffman tree, the `Fork` nodes, represent a set
    * containing all the characters present in the leaves below it.
    * The weight of a `Fork` node is the sum of the weights of these leaves.
    */
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

  // Part 1: Basics

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

  // Part 2: Generating Huffman trees

  /**
    * In this assignment, we are working with lists of characters.
    * This function allows you to easily create a character list from a given
    * string.
    */
  def string2Chars(str: String): List[Char] = str.toList

  /**
    * This function computes for each unique character in the list `chars` the
    * number of times it occurs.
    * For example, the invocation
    *
    *   times(List('a', 'b', 'a'))
    *
    * should return the following (the order of the resulting list is not
    * important):
    *
    *   List(('a', 2), ('b', 1))
    *
    * .
    * The type `List[(Char, Int)]` denotes a list of pairs, where each pair
    * consists of a character and an integer.
    * Pairs can be constructed easily using parentheses:
    *
    *   val pair: (Char, Int) = ('c', 1)
    *
    * .
    * In order to access the two elements of a pair, you can use the accessors
    * `_1` and `_2`:
    *
    *   val theChar = pair._1
    *   val theInt  = pair._2
    *
    * .
    * Another way to deconstruct a pair is using pattern matching:
    *
    *   pair match {
    *     case (theChar, theInt) =>
    *       println("character is: "+ theChar)
    *       println("integer is  : "+ theInt)
    *   }
    *
    * .
    */
  def times(chars: List[Char]): List[(Char, Int)] =
    chars
      .foldLeft(Map[Char, Int]())(
        (acc, c) => acc.updated(c, acc.getOrElse(c,0) + 1)
      )
      .toList

  /**
    * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
    *
    * The returned list should be ordered by ascending weights (i.e. the
    * head of the list should have the smallest weight), where the weight
    * of a leaf is the frequency of the character.
    */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    freqs
      .sortBy({case (c, w) => w})
      .map({case (c, w) => Leaf(c, w)})

  /**
    * Checks whether the list `trees` contains only one single code tree.
    */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case _ :: Nil => true
    case _ => false
  }

  /**
    * The parameter `trees` of this function is a list of code trees ordered by
    * ascending weights.
    *
    * This function takes the first two elements of the list `trees` and
    * combines them into a single `Fork` node. This node is then added back into
    * the remaining elements of `trees` at a position such that the ordering by
    * weights is preserved.
    *
    * If `trees` is a list of less than two elements, that list should be
    * returned unchanged.
    */
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

  /**
    * This function will be called in the following way:
    *
    *   until(singleton, combine)(trees)
    *
    * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer
    * to the two functions defined above.
    *
    * In such an invocation, `until` should call the two functions until the
    * list of code trees contains only one single tree, and then return that
    * singleton list.
    *
    * Hint: before writing the implementation,
    * - start by defining the parameter types such that the above example
    *   invocation is valid.
    *   The parameter types of `until` should match the argument types of the
    *   example invocation.
    *   Also define the return type of the `until` function.
    * - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
    */
  def until[A](p: A => Boolean, f: A => A)(x: A): A =
    if (p(x)) x else until(p, f)(f(x))

  /**
    * This function creates a code tree which is optimal to encode the text
    * `chars`.
    *
    * The parameter `chars` is an arbitrary text. This function extracts the
    * character frequencies from that text and creates a code tree based on
    * them.
    */
  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  // Part 3: Decoding

  type Bit = Int

  /**
    * This function decodes the bit sequence `bits` using the code tree `tree`
    * and returns the resulting list of characters.
    */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

    def helper(
      subtree: CodeTree,
      acc: List[Char],
      rest: List[Bit]
    ): List[Char] = {
      ???
    }

    helper(tree, List(), bits)
  }

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = ???

  // Part 4a: Encoding using Huffman tree

  /**
    * This function encodes `text` using the code tree `tree` into a sequence of
    * bits.
    */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = ???

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
    * This function returns the bit sequence that represents the character
    * `char` in the code table `table`.
    */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = ???

  /**
    * Given a code tree, create a code table which contains, for every character
    * in the code tree, the sequence of bits representing that character.
    *
    * Hint: think of a recursive solution: every sub-tree of the code tree
    * `tree` is itself a valid code tree that can be represented as a code
    * table.
    * Using the code tables of the sub-trees, think of how to build the code
    * table for the entire tree.
    */
  def convert(tree: CodeTree): CodeTable = ???

  /**
    * This function takes two code tables and merges them into one.
    * Depending on how you use it in the `convert` method above, this merge
    * method might also do some transformations on the two parameter code
    * tables.
    */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???

  /**
    * This function encodes `text` according to the code tree `tree`.
    *
    * To speed up the encoding process, it first converts the code tree to a
    * code table and then uses it to perform the actual encoding.
    */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = ???
}
