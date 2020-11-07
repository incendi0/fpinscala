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

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

   def length[A](as: List[A]): Int =
     foldRight(as, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, a) => Cons(a, acc))

  def foldRightViaFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((acc, a) => f(a, acc))

  def appendViaFoldRight[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)(Cons(_, _))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(_, t) => filter(t)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(map(as)(f), Nil: List[B])(append)

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List.apply(a) else Nil)

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case _ => Nil
    }

  @tailrec
  def hasSequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
        case _ => false
      }
    }

    sup match {
      case Nil => sub == Nil
      case s if startsWith(s, sup) => true
      case Cons(_, t) => hasSequence(t, sub)
    }
  }

}