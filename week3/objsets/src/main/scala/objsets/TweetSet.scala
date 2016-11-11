package objsets

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

sealed trait TweetZipper extends TweetSet {

  val data = this

  override def filter(p: Tweet => Boolean): TweetZipper = TZ.filter(this)(p)
  override def union(that: TweetSet): TweetZipper = TZ.union(this)(that.data)
  override def mostRetweeted: Tweet = TZ.mostRetweeted(this)
  override def descendingByRetweet: TweetList = TZ.descendingByRetweet(this)
  override def incl(tweet: Tweet): TweetZipper = TZ.incl(this)(tweet)
  override def remove(tweet: Tweet): TweetZipper = TZ.remove(this)(tweet)
  override def contains(tweet: Tweet): Boolean = TZ.contains(this)(tweet)
  override def foreach(f: Tweet => Unit): Unit = TZ.foreach(this)(f)
}

case object TZEmpty extends TweetZipper

sealed trait TZNonEmpty extends TweetZipper

case class TZSingleton(elem: Tweet) extends TZNonEmpty

sealed trait TZLeftable extends TZNonEmpty

sealed trait TZRightable extends TZNonEmpty

case class TZLeftEnd(elem: Tweet, right: List[Tweet]) extends TZRightable {
  require(right.nonEmpty)
}

case class TZRightEnd(left: List[Tweet], elem: Tweet) extends TZLeftable {
  require(left.nonEmpty)
}

case class TZMid(left: List[Tweet], elem: Tweet, right: List[Tweet])
  extends TZLeftable with TZRightable {
  require(left.nonEmpty && right.nonEmpty)
}

object TZ {

  private def stepLeft(tz: TZLeftable): TZRightable = tz match {

    case TZMid(l :: ls, e, right) =>
      if (ls.nonEmpty)
        TZMid(ls, l, e :: right)
      else
        TZLeftEnd(l, e :: right)

    case TZRightEnd(l :: ls, e) =>
      if (ls.nonEmpty)
        TZMid(ls, l, List(e))
      else
        TZLeftEnd(l, List(e))
  }

  private def stepRight(tz: TZRightable): TZLeftable = tz match {

    case TZLeftEnd(e, r :: rs) =>
      if (rs.nonEmpty)
        TZMid(List(e), r, rs)
      else
        TZRightEnd(List(e), r)

    case TZMid(left, e, r :: rs) =>
      if (rs.nonEmpty)
        TZMid(e :: left, r, rs)
      else
        TZRightEnd(e :: left, r)
  }

  private def marchLeft(tz: TweetZipper): TweetZipper = tz match {

    case TZEmpty =>
      tz

    case TZSingleton(_) =>
      tz

    case TZLeftEnd(_, _) =>
      tz

    case tz@TZRightEnd(_, _) =>
      marchLeft(stepLeft(tz))

    case tz@TZMid(_, _, _) =>
      marchLeft(stepLeft(tz))
  }

  private def marchRight(tz: TweetZipper): TweetZipper = tz match {

    case TZEmpty =>
      tz

    case TZSingleton(_) =>
      tz

    case tz@TZLeftEnd(_, _) =>
      marchRight(stepRight(tz))

    case TZRightEnd(_, _) =>
      tz

    case tz@TZMid(_, _, _) =>
      marchRight(stepRight(tz))
  }

  private def nub(tz: TweetZipper): TweetZipper = tz match {

    case TZEmpty =>
      throw new java.util.NoSuchElementException

    case TZSingleton(_) =>
      TZEmpty

    case TZLeftEnd(_, r1 :: rs) =>
      if (rs.nonEmpty)
        TZLeftEnd(r1, rs)
      else
        TZSingleton(r1)

    case TZMid(left@(l :: ls), _, right@(r :: rs)) =>
      if (ls.nonEmpty)
        TZMid(ls, l, right)
      else if (rs.nonEmpty)
        TZMid(left, r, rs)
      else
        TZLeftEnd(l, right)

    case TZRightEnd(l :: ls, _) =>
      if (ls.nonEmpty)
        TZRightEnd(ls, l)
      else
        TZSingleton(l)
  }

  private def get(tz: TweetZipper): (Tweet, TweetZipper) = tz match {

    case TZEmpty =>
      throw new java.util.NoSuchElementException

    case TZSingleton(e) =>
      (e, nub(tz))

    case TZLeftEnd(e, _) =>
      (e, nub(tz))

    case TZRightEnd(_, e) =>
      (e, nub(tz))

    case TZMid(_, e, _) =>
      (e, nub(tz))
  }

  def toLeftList(tz: TweetZipper): List[Tweet] = {

    val tz1 = marchRight(tz)

    tz1 match {

      case TZEmpty =>
        List()

      case TZSingleton(e) =>
        List(e)

      case TZRightEnd(l, e) =>
        e :: l
    }
  }

  def toRightList(tz: TweetZipper): List[Tweet] = {

    val tz1 = marchLeft(tz)

    tz1 match {

      case TZEmpty =>
        List()

      case TZSingleton(e) =>
        List(e)

      case TZLeftEnd(e, r) =>
        e :: r
    }
  }

  def filter(tz: TweetZipper)(p: Tweet => Boolean): TweetZipper = {

    val tz1 = marchLeft(tz)

    tz1 match {

      case TZEmpty =>
        TZEmpty

      case TZSingleton(e) =>
        if (p(e))
          TZSingleton(e)
        else
          TZEmpty

      case TZLeftEnd(e, l) => {
        val filteredList = (e :: l).filter(p)

        if (filteredList.nonEmpty)
          if (filteredList.tail.nonEmpty)
            TZLeftEnd(filteredList.head, filteredList.tail)
          else
            TZSingleton(filteredList.head)
        else
          TZEmpty
      }
    }
  }

  def union(tz: TweetZipper)(tz2: TweetZipper): TweetZipper = {

    def helper(
                acc: TweetZipper,
                rest1: TweetZipper,
                rest2: TweetZipper
              ): TweetZipper = (rest1, rest2) match {

      case (TZEmpty, TZEmpty) =>
        acc

      case (TZEmpty, _) =>
        val (e, n) = get(rest2)
        helper(incl(acc)(e), TZEmpty, n)

      case (_, TZEmpty) =>
        val (e, n) = get(rest1)
        helper(incl(acc)(e), TZEmpty, n)

      case (_, _) =>
        val (e1, n1) = get(rest1)
        val (e2, n2) = get(rest2)
        if (e1.text < e2.text)
          helper(incl(acc)(e1), n1, rest2)
        else
          helper(incl(acc)(e2), rest1, n2)
    }

    helper(TZEmpty, marchLeft(tz), marchLeft(tz2))
  }

  def mostRetweeted(tz: TweetZipper): Tweet = {

    val tz1 = marchLeft(tz)

    tz1 match {

      case TZEmpty =>
        throw new java.util.NoSuchElementException

      case TZSingleton(e) =>
        e

      case TZLeftEnd(e, l) =>
        (e :: l).maxBy(_.retweets)
    }
  }

  def descendingByRetweet(tz: TweetZipper): TweetList = {

    val tz1 = marchLeft(tz)

    tz1 match {

      case TZEmpty =>
        Nil

      case TZSingleton(e) =>
        Cons(e, Nil)

      case TZLeftEnd(e, l) =>
        TL.fromList((e :: l).sortBy(_.retweets))
    }
  }

  def incl(tz: TweetZipper)(t: Tweet): TweetZipper = tz match {

    case TZEmpty =>
      TZSingleton(t)

    case TZSingleton(e) =>
      if (t.text < e.text)
        TZLeftEnd(t, List(e))
      else if (t.text > e.text)
        TZRightEnd(List(e), t)
      else
        tz

    case tz@TZLeftEnd(e, r) =>
      if (t.text < e.text)
        TZLeftEnd(t, e :: r)
      else if (t.text > e.text)
        incl(stepRight(tz))(t)
      else
        tz

    case tz@TZRightEnd(l, e) =>
      if (t.text < e.text)
        incl(stepLeft(tz))(t)
      else if (t.text > e.text)
        TZRightEnd(e :: l, t)
      else
        tz

    case tz@TZMid(l, e, r) =>
      if (t.text < e.text)
        incl(stepLeft(tz))(t)
      else if (t.text > e.text)
        incl(stepRight(tz))(t)
      else
        tz
  }

  def remove(tz: TweetZipper)(t: Tweet): TweetZipper = {

    def helper(tz1: TweetZipper): TweetZipper = tz1 match {

      case TZSingleton(e) =>
        TZEmpty

      case tz1@TZLeftEnd(e, _) =>
        if (t.text == e.text)
          nub(tz1)
        else
          helper(stepRight(tz1))

      case tz1@TZRightEnd(_, e) =>
        if (t.text == e.text)
          nub(tz1)
        else
          helper(stepLeft(tz1))

      case tz1@TZMid(_, e, _) =>
        if (t.text < e.text)
          helper(stepLeft(tz1))
        else if (t.text > e.text)
          helper(stepRight(tz1))
        else
          nub(tz1)
    }

    if (contains(tz)(t))
      helper(tz)
    else
      tz
  }

  def contains(tz: TweetZipper)(t: Tweet): Boolean = tz match {

    case TZEmpty =>
      false

    case TZSingleton(e) =>
      t.text == e.text

    case tz@TZLeftEnd(e, _) =>
      if (t.text < e.text)
        false
      else
        t.text == e.text || contains(nub(tz))(t)

    case tz@TZRightEnd(_, e) =>
      if (t.text > t.text)
        false
      else
        t.text == e.text || contains(nub(tz))(t)

    case tz@TZMid(l, e, r) =>
      if (t.text < e.text)
      // drop the right branch, recurse
        contains(TZRightEnd(l, e))(t)
      else if (t.text > e.text)
      // drop the left branch, recurse
        contains(TZLeftEnd(e, r))(t)
      else
        t.text == e.text
  }

  def foreach(tz: TweetZipper)(a: Tweet => Unit): Unit = tz match {

    case TZEmpty =>
      ()

    case TZSingleton(e) =>
      a(e)

    case tz@TZLeftEnd(e, _) =>
      a(e)
      foreach(nub(tz))(a)

    case tz@TZRightEnd(_, _) =>
      foreach(stepLeft(tz))(a)

    case tz@TZMid(_, _, _) =>
      foreach(stepLeft(tz))(a)
  }
}
