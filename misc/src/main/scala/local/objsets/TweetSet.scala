package local.objsets

import TweetReader._

case class Tweet(user: String, text: String, retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

trait TweetSet {

  val data: TweetZipper

  def filter(p: Tweet => Boolean): TweetSet = data.filter(p)
  def union(that: TweetSet): TweetSet = data.union(that)
  def mostRetweeted: Tweet = data.mostRetweeted
  def descendingByRetweet: TweetList = data.descendingByRetweet
  def incl(tweet: Tweet): TweetSet = data.incl(tweet)
  def remove(tweet: Tweet): TweetSet = data.remove(tweet)
  def contains(tweet: Tweet): Boolean = data.contains(tweet)
  def foreach(f: Tweet => Unit): Unit = data.foreach(f)
}

class Empty extends TweetSet {
  val data = TZEmpty
}

class NonEmpty(
  elem: Tweet,
  left: TweetSet,
  right: TweetSet
) extends TweetSet {

  val leftList = TZ.toLeftList(left.data)
  val rightList = TZ.toRightList(right.data)

  val data =
    if (leftList.nonEmpty && rightList.nonEmpty)
      TZMid(leftList, elem, rightList)
    else if (leftList.nonEmpty)
      TZRightEnd(leftList, elem)
    else if (rightList.nonEmpty)
      TZLeftEnd(elem, rightList)
    else
      TZSingleton(elem)
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

  def fromList(list: List[Tweet]): TweetList = {

    def helper(acc: TweetList, rest: List[Tweet]): TweetList = rest match {
      case scala.collection.immutable.Nil =>
        acc
      case h :: t =>
        helper(Cons(h, acc), t)
    }

    helper(Nil, list)
  }

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
    case Nil => ()
    case Cons(e, r) => f(e); foreach(r)(f)
  }
}

object GoogleVsApple {

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet =
    allTweets.filter(tweet => google.exists(term => tweet.text.contains(term)))

  lazy val appleTweets: TweetSet =
    allTweets.filter(tweet => apple.exists(term => tweet.text.contains(term)))

  lazy val trending: TweetList =
    googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  GoogleVsApple.trending.foreach(println)
}
