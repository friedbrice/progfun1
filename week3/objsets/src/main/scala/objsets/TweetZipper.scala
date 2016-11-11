package objsets

sealed trait TweetZipper {
  def filter(p: Tweet => Boolean): TweetZipper = TZ.filter(this)(p)
  def union(that: TweetZipper): TweetZipper = TZ.union(this)(that)
  def mostRetweeted: Tweet = TZ.mostRetweeted(this)
  def descendingByRetweet: TweetList = TZ.descendingByRetweet(this)
  def incl(tweet: Tweet): TweetZipper = TZ.incl(this)(tweet)
  def remove(tweet: Tweet): TweetZipper = TZ.remove(this)(tweet)
  def contains(tweet: Tweet): Boolean = TZ.contains(this)(tweet)
  def foreach(f: Tweet => Unit): Unit = TZ.foreach(this)(f)
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

  private def nub(tz: TZNonEmpty): TweetZipper = tz match {

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

  def filter(tz: TweetZipper)(p: Tweet => Boolean): TweetZipper = ???

  def union(tz: TweetZipper)(tz2: TweetZipper): TweetZipper = ???

  def mostRetweeted(tz: TweetZipper): Tweet = ???

  def descendingByRetweet(tz: TweetZipper): TweetList = ???

  def incl(tz: TweetZipper)(t: Tweet): TweetZipper = ???

  def remove(tz: TweetZipper)(t: Tweet): TweetZipper = ???

  def contains(tz: TweetZipper)(t: Tweet): Boolean = tz match {

    case TZEmpty =>
      false

    case TZSingleton(e) =>
      t.text == e.text

    case tz@TZLeftEnd(e, r) =>
      if (t.text < e.text)
        false
      else
        t.text == e.text || contains(nub(tz))(t)

    case tz@TZRightEnd(l, e) =>
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

    case tz@TZLeftEnd(e, r) =>
      a(e)
      foreach(nub(tz))(a)

    case tz@TZRightEnd(l, e) =>
      foreach(stepLeft(tz))(a)

    case tz@TZMid(l, e, r) =>
      foreach(stepLeft(tz))(a)
  }
}
