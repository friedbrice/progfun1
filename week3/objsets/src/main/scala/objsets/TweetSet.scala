package objsets

import TweetReader._

/**
 * A class to represent tweets.
 */
case class Tweet(user: String, text: String, retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
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
sealed trait TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = TS.filter(this)(p)

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet = TS.union(this)(that)

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet = TS.mostRetweeted(this)

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList = TS.descendingByRetweet(this)

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet = TS.incl(this)(tweet)

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet = TS.remove(this)(tweet)

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean = TS.contains(this)(tweet)

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit = TS.foreach(this)(f)
}

case class Empty() extends TweetSet

case class NonEmpty(
  elem: Tweet,
  left: TweetSet,
  right: TweetSet
) extends TweetSet

object TS {

  def filter(ts: TweetSet)(p: Tweet => Boolean): TweetSet = ts match {
    case Empty() =>
      Empty()
    case NonEmpty(e, l, r) => {
      if (p(e))
        // TODO: Remove union from this call
        incl(filter(union(l)(r))(p))(e)
      else
        // TODO: Remove union from this call
        filter(union(l)(r))(p)
    }
  }

  def union(ts: TweetSet)(ts2: TweetSet): TweetSet = ts match {
    case Empty() =>
      ts2
    case NonEmpty(e, l, r) =>
      // TODO: is there a faster implementation?
      incl(union(union(l)(r))(ts2))(e)
  }

  def mostRetweeted(ts: TweetSet): Tweet = ts match {
    case Empty() => throw new java.util.NoSuchElementException
    case NonEmpty(e, l, r) => {

      def helper(acc: Tweet, rest: TweetSet): Tweet = rest match {
        case Empty() => acc
        case NonEmpty(e2, l2, r2) =>
          if (e2.text > acc.text)
            // TODO: remove union
            helper(e2, union(l2)(r2))
          else
            // TODO: remove union
            helper(acc, union(l2)(r2))
      }

      // TODO: remove union
      helper(e, union(l)(r))
    }
  }

  def descendingByRetweet(ts: TweetSet): TweetList = {

    def toList: List[Tweet] = {

      def helper(acc: List[Tweet], rest: TweetSet): List[Tweet] = rest match {
        case Empty() =>
          acc
        case NonEmpty(e, l, r) =>
          // TODO: remove union
          helper(e :: acc, union(l)(r))
      }

      helper(List(), ts)
    }

    def toTweetList(lt: List[Tweet]): TweetList = {

      def helper(acc: TweetList, rest: List[Tweet]): TweetList = rest match {
        case List() =>
          acc
        case (h :: t) =>
          helper(Cons(h, acc), t)
      }

      helper(Nil, lt)
    }

    toTweetList(toList.sortBy(_.retweets))
  }

  def incl(ts: TweetSet)(t: Tweet): TweetSet = ts match {
    case Empty() =>
      NonEmpty(t, Empty(), Empty())
    case NonEmpty(e, l, r) => {
      if (t.text < e.text)
        NonEmpty(e, incl(l)(t), r)
      else if (t.text > e.text)
        NonEmpty(e, l, incl(r)(t))
      else
        ts
    }
  }

  def remove(ts: TweetSet)(t: Tweet): TweetSet = ts match {
    case Empty() =>
      Empty()
    case NonEmpty(e, l, r) => {
      if (t.text < e.text)
        NonEmpty(e, remove(l)(t), r)
      else if (t.text > e.text)
        NonEmpty(e, l, remove(r)(t))
      else
        // TODO: remove union here
        union(l)(r)
    }
  }

  def contains(ts: TweetSet)(t: Tweet): Boolean = ts match {
    case Empty() =>
      false
    case NonEmpty(e, l, r) =>
      // TODO: remove union
      t == e || contains(union(l)(r))(t)
  }

  def foreach(ts: TweetSet)(f: Tweet => Unit): Unit = ts match {
    case Empty() =>
      ()
    case NonEmpty(e, l, r) => {
      f(e)
      // TODO: remove union
      foreach(union(l)(r))(f)
    }
  }
}

sealed trait TweetList {
  def incl(t: Tweet): TweetList = TL.incl(this)(t)
  def head: Tweet = TL.head(this)
  def tail: TweetList = TL.tail(this)
  def isEmpty: Boolean = TL.isEmpty(this)
  def foreach(f: Tweet => Unit): Unit = TL.foreach(this)(f)
}

case object Nil extends TweetList

case class Cons(elem: Tweet, rest: TweetList) extends TweetList

object TL {

  def incl(tl: TweetList)(t: Tweet): TweetList = tl match {
    case Nil => Cons(t, Nil)
    case _ => Cons(t, tl)
  }

  def head(tl: TweetList): Tweet = tl match {
    case Nil => throw new java.util.NoSuchElementException
    case Cons(e, _) => e
  }

  def tail(tl: TweetList): TweetList = tl match {
    case Nil => throw new java.util.NoSuchElementException
    case Cons(_, r) => r
  }

  def isEmpty(tl: TweetList): Boolean = tl match {
    case Nil => true
    case _ => false
  }

  def foreach(tl: TweetList)(f: Tweet => Unit): Unit = tl match {
    case Nil =>
    case Cons(e, r) => f(e); foreach(r)(f)
  }
}

object GoogleVsApple {

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet =
    google
      .map(term => allTweets.filter(tweet => tweet.text.contains(term)))
      .reduce(_.union(_))
  
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val appleTweets: TweetSet =
    apple
      .map(term => allTweets.filter(tweet => tweet.text.contains(term)))
      .reduce(_.union(_))

  lazy val trending: TweetList =
    googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending.foreach(println)
}
