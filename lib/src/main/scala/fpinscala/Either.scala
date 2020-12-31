package fpinscala.errorhandling

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case _ => _
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case _ => _
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case _ => _
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
//    this.flatMap(ea => b.map(eb => f(ea, eb)))
    for {
      ea <- this
      eb <- b
    } yield f(ea, eb)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    es.foldRight[Either[E, List[A]]](Right(Nil))((a, acc) => a.map2(acc)(_ :: _))
//    traverse(es)(identity)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, acc) => f(a).map2(acc)(_ :: _))
//    sequence(as.map(f))
  }
}
