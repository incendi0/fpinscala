package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("tail on empty list")
    case Cons(_, tail) => tail
  }

  def setHead[A, B >: A](a: A, bs: List[B]): List[B] = bs match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, tail) => Cons(a, tail)
  }

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Cons(_, tail) if n > 0 => drop(tail, n - 1)
    case _ => as
  }

  @tailrec
  def dropWhile[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case _ => as
  }

  def append[A](as1: List[A], as2: List[A]): List[A] = as1 match {
    case Cons(head, tail) => Cons(head, append(tail, as2))
    case _ => as2
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil => sys.error("init on empty list")
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }
}