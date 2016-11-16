package patmat

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  object TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(
      Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5),
      Leaf('d',4),
      List('a','b','d'),
      9
    )
	}

  def checkWeight(tree: CodeTree)(char: Char): Int = tree match {
    case Leaf(_, w) => w
    case Fork(l, r, cs, _) =>
      if (chars(l).contains(char))
        checkWeight(l)(char)
      else if (chars(r).contains(char))
        checkWeight(r)(char)
      else
        0
  }

  test("weight of a larger tree") {
    // given
    val tree = TestTrees.t1
    val expectedOutput = 5

    // when
    val actualOutput = weight(tree)

    // then
    assert(actualOutput === expectedOutput)
  }

  test("chars of a larger tree") {
    // given
    val tree = TestTrees.t2
    val expectedOutput = List('a', 'b', 'd')

    // when
    val actualOutput = chars(tree)

    // then
    assert(actualOutput === expectedOutput)
  }

  test("string2chars(\"hello, world\")") {
    // given
    val input = "hello, world"
    val expectedOutput = List(
      'h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'
    )

    // when
    val actualOutput = string2Chars(input)

    // then
    assert(expectedOutput === actualOutput)
  }

  test("makeOrderedLeafList for some frequency table") {
    // given
    val input = List(('t', 2), ('e', 1), ('x', 3))
    val expectedOutput = List(Leaf('e',1), Leaf('t',2), Leaf('x',3))

    // when
    val actualOutput = makeOrderedLeafList(input)

    // then
    assert(actualOutput === expectedOutput)
  }

  test("combine of some leaf list") {
    // given
    val input = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val expectedOutput =
      List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4))

    // when
    val actualOutput = combine(input)

    // then
    assert(actualOutput === expectedOutput)
  }

  test("decode and encode a very short text should be identity") {
    // given
    val tree = TestTrees.t1
    val input = "ab".toList

    // when
    val output = decode(tree, encode(tree)(input))

    // then
    assert(output === input)
  }

  test("decode and encode a longer message should be identity") {
    // given
    val tree = HuffmanData.frenchCode
    val chars = "allthesmallthings".toList

    // when
    val result = decode(tree, encode(tree)(chars))

    // then
    assert(result === chars)
  }

  test("decode and encode a longer longer message should be identity") {
    // given
    val tree = HuffmanData.frenchCode
    val chars = "thequickbrownfoxjumpsoverthelazydog".toList

    // when
    val result = decode(tree, encode(tree)(chars))

    // then
    assert(result === chars)
  }

  test("decode and quickEncode should be inverses") {
    // given
    val tree = HuffmanData.frenchCode
    val chars = "thequickbrownfoxjumpsoverthelazydog".toList

    // when
    val result = decode(tree, quickEncode(tree)(chars))

    // then
    assert(result === chars)
  }

  test("createCodeTree creates a codeTree") {
    // given
    val chars = "thequickbrownfoxjumpsoverthelazydog".toList

    // when
    val result = createCodeTree(chars)

    // then
    assert(result !== null)
  }

  test("createCodeTree gets the weights right") {
    // given
    val chars = "gherwaufjsa".toList

    // when
    val result = createCodeTree(chars)

    // then
    assert(checkWeight(result)('g') === 1)
    assert(checkWeight(result)('h') === 1)
    assert(checkWeight(result)('e') === 1)
    assert(checkWeight(result)('r') === 1)
    assert(checkWeight(result)('w') === 1)
    assert(checkWeight(result)('a') === 2)
    assert(checkWeight(result)('u') === 1)
    assert(checkWeight(result)('f') === 1)
    assert(checkWeight(result)('j') === 1)
    assert(checkWeight(result)('s') === 1)
    assert(checkWeight(result)('q') === 0)
    assert(checkWeight(result)('l') === 0)
  }

  test("a text can be used to generate a code tree, and that code tree can be used to encode and decode that same text") {
    // given
    val chars =
      """
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
        |tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
        |veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
        |commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
        |velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
        |occaecat cupidatat non proident, sunt in culpa qui officia deserunt
        |mollit anim id est laborum.
        |""".stripMargin.toList

    // when
    val tree = createCodeTree(chars)
    val encoded = encode(tree)(chars)
    val decoded = decode(tree, encoded)

    // then
    assert(decoded.mkString === chars.mkString)
  }

  test("a text can be used to generate a code tree, and that code tree can be used to encode and decode other texts") {
    // given
    val msg1 =
      """
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
        |tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
        |veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
        |commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
        |velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
        |occaecat cupidatat non proident, sunt in culpa qui officia deserunt
        |mollit anim id est laborum.
        |""".stripMargin.toList
    val msg2 =
      """
        |Sed ut perspiciatis, unde omnis iste natus error sit voluptatem
        |accusantium doloremque laudantium, totam rem aperiam eaque ipsa, quae
        |ab illo inventore veritatis et quasi architecto beatae vitae dicta
        |sunt, explicabo. Nemo enim ipsam voluptatem, quia voluptas sit,
        |aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos,
        |qui ratione voluptatem sequi nesciunt, neque porro quisquam est, qui
        |dolorem ipsum, quia dolor sit amet consectetur adipiscing velit, sed
        |quia non numquam do eius modi tempora incididunt, ut labore et dolore
        |magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis
        |nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut
        |aliquid ex ea commodi consequatur. Quis autem vel eum iure
        |reprehenderit, qui in ea voluptate velit esse, quam nihil molestiae
        |consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla
        |pariatur.
        |""".stripMargin.toList

    // when
    val treeA = createCodeTree(msg1)
    val treeB = createCodeTree(msg2)
    val treeC = createCodeTree(msg1 ++ msg2)
    val enc1A = encode(treeA)(msg1)
    val enc1C = encode(treeC)(msg1)
    val enc2B = encode(treeB)(msg2)
    val enc2C = encode(treeC)(msg2)
    val dec1A = decode(treeA, enc1A)
    val dec1C = decode(treeC, enc1C)
    val dec2B = decode(treeB, enc2B)
    val dec2C = decode(treeC, enc2C)

    // then
    assert(dec1A === msg1)
    assert(dec1C === msg1)
    assert(dec2B === msg2)
    assert(dec2C === msg2)
  }

  test("same as above, but with quickEncode") {
    // given
    val msg1 =
      """
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
        |tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
        |veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
        |commodo consequat. Duis aute irure dolor in reprehenderit in voluptate
        |velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint
        |occaecat cupidatat non proident, sunt in culpa qui officia deserunt
        |mollit anim id est laborum.
        |""".stripMargin.toList
    val msg2 =
      """
        |Sed ut perspiciatis, unde omnis iste natus error sit voluptatem
        |accusantium doloremque laudantium, totam rem aperiam eaque ipsa, quae
        |ab illo inventore veritatis et quasi architecto beatae vitae dicta
        |sunt, explicabo. Nemo enim ipsam voluptatem, quia voluptas sit,
        |aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos,
        |qui ratione voluptatem sequi nesciunt, neque porro quisquam est, qui
        |dolorem ipsum, quia dolor sit amet consectetur adipiscing velit, sed
        |quia non numquam do eius modi tempora incididunt, ut labore et dolore
        |magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis
        |nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut
        |aliquid ex ea commodi consequatur. Quis autem vel eum iure
        |reprehenderit, qui in ea voluptate velit esse, quam nihil molestiae
        |consequatur, vel illum, qui dolorem eum fugiat, quo voluptas nulla
        |pariatur.
        |""".stripMargin.toList

    // when
    val treeA = createCodeTree(msg1)
    val treeB = createCodeTree(msg2)
    val treeC = createCodeTree(msg1 ++ msg2)
    val qEncoderA = quickEncode(treeA)(_)
    val qEncoderB = quickEncode(treeB)(_)
    val qEncoderC = quickEncode(treeC)(_)
    val enc1A = qEncoderA(msg1)
    val enc1C = qEncoderC(msg1)
    val enc2B = qEncoderB(msg2)
    val enc2C = qEncoderC(msg2)
    val dec1A = decode(treeA, enc1A)
    val dec1C = decode(treeC, enc1C)
    val dec2B = decode(treeB, enc2B)
    val dec2C = decode(treeC, enc2C)

    // then
    assert(dec1A === msg1)
    assert(dec1C === msg1)
    assert(dec2B === msg2)
    assert(dec2C === msg2)
  }
}
