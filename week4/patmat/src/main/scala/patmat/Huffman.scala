package patmat

object Huffman extends App {

  val frenchCode: CodeTree = HuffmanData.frenchCode

  val secret: List[Bit] = HuffmanData.secret

  /**
    * Write a function that returns the decoded secret
    */
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  println(decodedSecret.mkString)
}
