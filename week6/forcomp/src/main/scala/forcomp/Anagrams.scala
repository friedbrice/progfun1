package forcomp

object Anagrams {

  /**
    * A word is simply a `String`.
    */
  type Word = String

  /**
    * A sentence is a `List` of words.
    */
  type Sentence = List[Word]

  /**
    * `Occurrences` is a `List` of pairs of characters and positive
    *  integers saying how often the character appears.
    *  This list is sorted alphabetically w.r.t. to the character in
    *  each pair.
    *  All characters in the occurrence list are lowercase.
    *
    *  Any list of pairs of lowercase characters and their frequency
    *  which is not sorted is **not** an occurrence list.
    *
    *  Note: If the frequency of some character is zero, then that
    *  character should not be in the list.
    */
  type Occurrences = List[(Char, Int)]

  /**
    * The dictionary is simply a sequence of words.
    * It is predefined and obtained as a sequence using the utility
    * method `loadDictionary`.
    */
  val dictionary: List[Word] = loadDictionary

  /**
    * Symmetric addition of two `Occurrences`.
    */
  def combine(x: Occurrences, y: Occurrences): Occurrences = {

    def helper: (Map[Char, Int], (Char, Int)) => Map[Char, Int] = {
      case (acc, (c, n)) => acc.updated(c, acc.getOrElse(c, 0) + n)
    }

    y.foldLeft(x.toMap)(helper).toList.filter(_._2 > 0).sortBy(_._1)
  }

  /**
    * Subtracts occurrence list `y` from occurrence list `x`.
    *
    * The precondition is that the occurrence list `y` is a subset of
    * the occurrence list `x` -- any character appearing in `y` must
    * appear in `x`, and its frequency in `y` must be smaller or equal
    * than its frequency in `x`.
    *
    * Note: the resulting value is an occurrence - meaning it is sorted
    * and has no zero-entries.
    */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {

    def helper: (Map[Char, Int], (Char, Int)) => Map[Char, Int] = {
      case (acc, (c, n)) => acc.updated(c, acc(c) - n)
    }

    y.foldLeft(x.toMap)(helper).toList.filter(_._2 > 0).sortBy(_._1)
  }

  /**
    * Returns a (probably) non-sense 'String' that is a preimage of the
    * supplied `Occurrences` under `wordOccurrences`.
    */
  def makeString(occurrences: Occurrences): Word = {
    occurrences.flatMap({case (c, n) => List.fill(n)(c)}).mkString
  }

  /**
    * From a `Char`, derive a singleton `Occurrences`.
    */
  def charOccurrences(char: Char): Occurrences = {
    List((char.toLower, 1))
  }

  /**
    * Converts the word into its character occurrence list.
    *
    * Note: the uppercase and lowercase version of the character are
    * treated as the same character, and are represented as a lowercase
    * character in the occurrence list.
    *
    *  Note: you must use `groupBy` to implement this method!
    */
  def wordOccurrences(w: Word): Occurrences = {
    w.toList.foldRight(Map[Char, Int]())(
      (c, acc) => combine(charOccurrences(c), acc.toList).toMap
    ).toList.filter(_._2 > 0).sortBy(_._1)
  }

  /**
    * Converts a sentence into its character occurrence list.
    */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    s.foldRight(Map[Char, Int]())(
      (c, acc) => combine(wordOccurrences(c), acc.toList).toMap
    ).toList.filter(_._2 > 0).sortBy(_._1)
  }

  /**
    * The `dictionaryByOccurrences` is a `Map` from different
    * occurrences to a sequence of all the words that have that
    * occurrence count.
    * This map serves as an easy way to obtain all the anagrams of a
    * word given its occurrence list.
    *
    * For example, the word "eat" has the following character occurrence
    * list:
    *
    *     List(('a', 1), ('e', 1), ('t', 1))
    *
    * Incidentally, so do the words "ate" and "tea".
    *
    * This means that the `dictionaryByOccurrences` map will contain an
    * entry:
    *
    *     List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
    *
    */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {

    val occurrencesByWord = dictionary.map(w => (w, wordOccurrences(w)))
    val allOccurrences = occurrencesByWord.map(_._2)

    def getFiber(occ: Occurrences) = {
      val fiber = occurrencesByWord.filter(_._2 == occ).map(_._1)
      (occ, fiber)
    }

    allOccurrences.map(getFiber).toMap
  }

  /**
    * Interface to `dictionaryByOccurrences`
    */
  def wordLookup(occurrences: Occurrences): List[Word] = {
    dictionaryByOccurrences.getOrElse(occurrences, Nil)
  }

  /**
    * Returns all the anagrams of a given word.
    */
  def wordAnagrams(word: Word): List[Word] = {
    wordLookup(wordOccurrences(word))
  }

  /**
    * Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e.
    * `List(('k', 1), ('o', 1))` is a subset of
    * `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list
    * `List(('a', 2), ('b', 2))` are:
    *
    *     List(
    *       List(),
    *       List(('a', 1)),
    *       List(('a', 2)),
    *       List(('b', 1)),
    *       List(('a', 1), ('b', 1)),
    *       List(('a', 2), ('b', 1)),
    *       List(('b', 2)),
    *       List(('a', 1), ('b', 2)),
    *       List(('a', 2), ('b', 2))
    *     )
    *
    * Note that the order of the occurrence list subsets does not matter
    * -- the subsets in the example above could have been displayed in
    * some other order.
    */
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    def subseqs[A](list: List[A]): List[List[A]] = list match {
      case Nil => List(List())
      case x :: xs => {
        val xss = subseqs(xs)
        xss ++ xss.map(x :: _)
      }
    }

    subseqs(makeString(occurrences).toList)
      .map(cs => wordOccurrences(cs.mkString))
      .distinct
  }

  /**
    * Returns all possible extentions of the supplied `Sentence` using
    * all or part of the supplied `Occurrences`, along with the
    * remainder of the `Occurrences`.
    */
  def extend(x: (Sentence, Occurrences)): List[(Sentence, Occurrences)] = {

    val potentialWords = combinations(x._2).flatMap(wordLookup)

    def appendWord(w: Word): (Sentence, Occurrences) =
      (x._1 ++ List(w), subtract(x._2, wordOccurrences(w)))

    def prune(y: (Sentence, Occurrences)): Boolean =
      (y._2 == List()) || (combinations(y._2).flatMap(wordLookup) != List())

    potentialWords.map(appendWord).filter(prune)
  }

  /**
    * Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of
    * all the characters of all the words in the sentence, and producing
    * all possible combinations of words with those characters, such
    * that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have
    * to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram
    * of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order
    * are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and
    * `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its
    * anagrams for our dictionary:
    *
    *     List(
    *       List(en, as, my),
    *       List(en, my, as),
    *       List(man, yes),
    *       List(men, say),
    *       List(as, en, my),
    *       List(as, my, en),
    *       List(sane, my),
    *       List(Sean, my),
    *       List(my, en, as),
    *       List(my, as, en),
    *       List(my, sane),
    *       List(my, Sean),
    *       List(say, men),
    *       List(yes, man)
    *     )
    *
    * The different sentences do not have to be output in the order
    * shown above -- any order is fine as long as all the anagrams are
    * there.
    * Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the
    * dictionary, then the sentence is the anagram of itself, so it has
    * to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    def helper: List[Sentence] => List[(Sentence, Occurrences)] => List[Sentence]
    = acc => {
      case Nil => acc
      case rest => {

        val done = rest.filter(_._2 == Nil)
        val newAcc = acc ++ done.map(_._1)
        val notDone = rest.filter(_._2 != Nil)
        val newRest = notDone.flatMap(extend)

        helper(newAcc)(newRest)
      }
    }

    helper(List())(List((List(), sentenceOccurrences(sentence))))
  }
}
