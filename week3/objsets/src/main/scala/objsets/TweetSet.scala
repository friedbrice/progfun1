package objsets

import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
  def getRetweets = this.retweets
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The elements in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  def isEmpty: Boolean

  def getElem: Tweet

  def getLeft: TweetSet

  def getRight: TweetSet

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList

  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def isEmpty = true

  def getElem = throw new java.util.NoSuchElementException()

  def getLeft = throw new java.util.NoSuchElementException()

  def getRight = throw new java.util.NoSuchElementException()

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * The following methods are already implemented
   */

  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  /**
    * This method takes a predicate and returns a subset of all the elements
    * in the original set for which the predicate is true.
    *
    * Question: Can we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def filter(p: (Tweet) => Boolean): TweetSet = this

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def union(that: TweetSet): TweetSet = that

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException()

  /**
    * Returns a list containing all tweets of this set, sorted by retweet count
    * in descending order. In other words, the head of the resulting list should
    * have the highest retweet count.
    *
    * Hint: the method `remove` on TweetSet will be very useful.
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def descendingByRetweet: TweetList = Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def isEmpty = false

  def getElem = this.elem

  def getLeft = this.left

  def getRight = this.right

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet =
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  /**
    * This method takes a predicate and returns a subset of all the elements
    * in the original set for which the predicate is true.
    *
    * Question: Can we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def filter(p: (Tweet) => Boolean): TweetSet = {

    def helper(acc: TweetSet, rest: TweetSet): TweetSet =
      if (rest.isEmpty)
        acc
      else if (p(rest.getElem))
        helper(acc.incl(rest.getElem), rest.getLeft.union(rest.getRight))
      else
        helper(acc, rest.getLeft.union(rest.getRight))

    helper(new Empty, this)
  }

  /**
    * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def union(that: TweetSet): TweetSet =
    left.union(right).union(that).incl(elem)

  /**
    * Returns the tweet from this set which has the greatest retweet count.
    *
    * Calling `mostRetweeted` on an empty set should throw an exception of
    * type `java.util.NoSuchElementException`.
    *
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def mostRetweeted: Tweet = {

    def helper(acc: Tweet, ts: TweetSet): Tweet =
      if (ts.isEmpty)
        acc
      else if (ts.getElem.getRetweets > acc.getRetweets)
        helper(ts.getElem, ts.getLeft.union(ts.getRight))
      else
        helper(acc, ts.getLeft.union(ts.getRight))

    helper(elem, left.union(right))
  }

  /**
    * Returns a list containing all tweets of this set, sorted by retweet count
    * in descending order. In other words, the head of the resulting list should
    * have the highest retweet count.
    *
    * Hint: the method `remove` on TweetSet will be very useful.
    * Question: Should we implment this method here, or should it remain abstract
    * and be implemented in the subclasses?
    */
  def descendingByRetweet: TweetList = {

    def tweetSetToList(ts: TweetSet): List[Tweet] = {

      def helper(acc: List[Tweet], rest: TweetSet): List[Tweet] =
        if (rest.isEmpty)
          acc
        else
          helper(rest.getElem :: acc, rest.getLeft.union(rest.getRight))

      helper(List(), this)
    }

    def listToTweetList(ts: List[Tweet]): TweetList = {

      def helper(acc: TweetList, rest: List[Tweet]): TweetList =
        if (rest.isEmpty)
          acc
        else
          helper(acc.push(rest.head), rest.tail)

      helper(Nil, ts)
    }

    listToTweetList(tweetSetToList(this).sortBy(_.retweets))
  }
}

trait TweetList {
  def push(t: Tweet): TweetList
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def push(t: Tweet): TweetList = new Cons(t, Nil)
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def push(t: Tweet): TweetList = new Cons(t, this)
  def isEmpty = false
}

object GoogleVsApple {

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val allTweets: TweetSet = TweetReader.allTweets

  lazy val googleTweets: TweetSet =
    google
      .map(term => allTweets.filter(tweet => tweet.text.contains(term)))
      .reduce(_.union(_))

  lazy val appleTweets: TweetSet =
    apple
      .map(term => allTweets.filter(tweet => tweet.text.contains(term)))
      .reduce(_.union(_))
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList =
    googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending.foreach(println)
}
