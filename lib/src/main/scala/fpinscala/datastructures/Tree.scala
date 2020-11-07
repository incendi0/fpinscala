package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](ta: Tree[A]): Int = ta match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tr: Tree[Int]): Int = tr match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](ta: Tree[A]): Int = ta match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](ta: Tree[A])(f: A => B): Tree[B] = ta match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](ta: Tree[A])(f: A => B)(g: (B, B) => B): B = ta match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](ta: Tree[A]): Int =
    fold(ta)(_ => 1)(_ + _ + 1)

  def maximumViaFold(tr: Tree[Int]): Int =
    fold(tr)(identity)(_ max _)

  def depthViaFold[A](ta: Tree[A]): Int =
    fold(ta)(_ => 1)((l, r) => 1 + (l max r))

  def mapViaFold[A, B](ta: Tree[A])(f: A => B): Tree[B] =
    fold(ta)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}