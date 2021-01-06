package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Executors, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(() => a(es).get)
  }

  def _map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    Map2Future(af, bf, f)
  }

  private case class Map2Future[A, B, C](af: Future[A], bf: Future[B], f: (A, B) => C) extends Future[C] {

    @volatile var cache: Option[C] = None

    override def cancel(mayInterruptIfRunning: Boolean): Boolean =
      af.cancel(mayInterruptIfRunning) || bf.cancel(mayInterruptIfRunning)

    override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

    override def isDone: Boolean = cache.isDefined

    override def get(): C = compute(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.NANOSECONDS.convert(timeout, unit))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime()
        val a = af.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val end = System.nanoTime()
        val elapsed = end - start
        val b = bf.get(timeoutInNanos - elapsed, TimeUnit.NANOSECONDS)
        val ret = f(a, b)
        cache = Some(ret)
        ret
    }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((a, _) => a.sorted)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) unit(Vector())
    else if (ps.length == 1) map(ps.head)(a => Vector(a))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)
//    ps match {
//      case Nil => unit(Nil)
//      case h :: t => map2(h, fork(sequence(t)))(_ :: _)
//    }
//    ps.foldRight[Par[List[A]]](unit(List[Par[A]]()))((a, acc) => map2(a, acc)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] = as.map(asyncF(a => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten)
  }

  def equal[A](es: ExecutorService)(pa1: Par[A], pa2: Par[A]):Boolean = pa1(es).get == pa2(es).get

  def main(args: Array[String]): Unit = {
    val a = lazyUnit(42 +1)
    val es = Executors.newFixedThreadPool(1)
    // deadlock
    println(Par.equal(es)(a, fork(a)))
    es.shutdown()
  }
}
