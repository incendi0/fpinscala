package fpinscala.parallelism

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}

object Nonblocking {

  trait Future[+A] {
    private[parallelism] def apply(cb: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  object Par {

    def run[A](es: ExecutorService)(pa: Par[A]): A = {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      pa(es) {
        a => ref.set(a); latch.countDown()
      }
      latch.await()
      ref.get()
    }

    def unit[A](a: A): Par[A] = _ => (cb: A => Unit) => cb(a)

    def delay[A](a: => A): Par[A] = _ => (cb: A => Unit) => cb(a)

    def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
      override def call(): Unit = r
    })

    def fork[A](a: => Par[A]): Par[A] = es => (cb: A => Unit) => eval(es)(a(es)(cb))

    def async[A](f: (A => Unit) => Unit): Par[A] = _ => (cb: A => Unit) => f(cb)

    def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = es => (cb: C => Unit) => {
      var ar: Option[A] = None
      var br: Option[B] = None
      val combiner = Actor[Either[A, B]](es) {
        case Left(a) =>
          if (br.isDefined) eval(es)(cb(f(a, br.get)))
          else ar = Some(a)
        case Right(b) =>
          if (ar.isDefined) eval(es)(cb(f(ar.get, b)))
          else br = Some(b)
      }
      pa(es)(a => combiner ! Left(a))
      pb(es)(b => combiner ! Right(b))
    }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = es => (cb: B => Unit) => pa(es)(a => eval(es)(cb(f(a))))

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
      as match {
        case Nil => unit(Nil)
        case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
      }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2);
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] =
      sequence(as.map(asyncF(f)))

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars = as.map(asyncF(a => if (f(a)) List(a) else List()))
      map(sequence(pars))(_.flatten)
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => (cb: A => Unit) => cond(es) { b =>
      if (b) eval(es)(t(es)(cb))
      else eval(es)(f(es)(cb))
    }

    def choiceN[A](p: Par[Int])(ps: List[Par[A]]): Par[A] = es => (cb: A => Unit) => p(es) { idx =>
      eval(es)(ps(idx)(es)(cb))
    }

    def _choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => (cb: V => Unit) => {
      key(es) { k =>
        eval(es)(choices(k)(es)(cb))
      }
    }

    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => (cb: B => Unit) => pa(es)(a => f(a)(es)(cb))

    def chooser[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = flatMap(pa)(f)

    def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      flatMap(cond)(a => if (a) t else f)

    def choiceNViaFlatMap[A](p: Par[Int])(ps: List[Par[A]]): Par[A] =
      flatMap(p)(ps(_))

    def choiceMapViaFlatMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      flatMap(key)(choices(_))

    def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)

    def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))

    implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

    class ParOps[A](p: Par[A]) {
      def map[B](f: A => B): Par[B] = Par.map(p)(f)

      def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)

      def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)

      def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
    }

  }

}
