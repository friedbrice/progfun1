package local

import local.comparable._

package object tree_set {

  sealed trait TreeSet[+A]

  case object Leaf extends TreeSet[Nothing]

  case class Branch[A](
    data: A,
    left: TreeSet[A],
    right: TreeSet[A]
  ) extends TreeSet[A]

  def isLeaf[A](ts: TreeSet[A]): Boolean =
    ts match {
      case Leaf => false
      case _ => true
    }

  def contains[A: Comparable](ts: TreeSet[A], a: A): Boolean =
    ts match {
      case Leaf =>
        false
      case Branch(t, l, r) =>
        if (isLess(a, t))
          contains(l, a)
        else if (isMore(a, t))
          contains(r, a)
        else
          true
    }

  def insert[A: Comparable](ts: TreeSet[A], a: A): TreeSet[A] =
    ts match {
      case Leaf =>
        Branch(a, Leaf, Leaf)
      case Branch(t, l, r) =>
        if (isLess(a, t))
          Branch(t, insert(l, a), r)
        else if (isMore(a, t))
          Branch(t, l, insert(r, a))
        else
          ts
    }

  def remove[A: Comparable](ts: TreeSet[A], a: A): TreeSet[A] =
    ts match {
      case Leaf =>
        Leaf
      case Branch(t, l, r) =>
        if (isSame(a, t))
          union(l, r)
        else
          insert(remove(union(l, r), a), t)
    }

  def union[A: Comparable](ts1: TreeSet[A], ts2: TreeSet[A]): TreeSet[A] =
    ts1 match {
      case Leaf =>
        ts2
      case Branch(t, l, r) =>
        insert(union(union(l, r), ts2), t)
    }

  def intersection[A: Comparable](
    ts1: TreeSet[A],
    ts2: TreeSet[A]
  ): TreeSet[A] =
    ts1 match {
      case Leaf =>
        Leaf
      case Branch(t, l, r) =>
        if (contains(ts2, t))
          insert(intersection(union(l, r), ts2), t)
        else
          intersection(union(l, r), ts2)
    }

  def setminus[A: Comparable](ts1: TreeSet[A], ts2: TreeSet[A]): TreeSet[A] =
    (ts1, ts2) match {
      case (Leaf, _) =>
        Leaf
      case (_, Leaf) =>
        ts1
      case (_, Branch(t, l, r)) =>
        setminus(remove(ts1, t), union(l, r))
    }

  def toString[A](ts: TreeSet[A]): String =
    ts match {
      case Leaf =>
        "."
      case Branch(t, l, r) =>
        s"{${toString(l)}$t${toString(r)}"
    }
}
