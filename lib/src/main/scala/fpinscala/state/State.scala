package fpinscala.state

import fpinscala.state.State.{get, modify, sequence, unit}

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEFCE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, r) = rng.nextInt
    (if (x < 0) -(x + 1) else x, r)
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (d, r) = nonNegativeInt(rng)
    (d % 2 == 1, r)
  }

  def doubleRNG(rng: RNG): (Double, RNG) = {
    val (x, r) = nonNegativeInt(rng)
    (x / (Int.MaxValue.toDouble + 1), r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (x, r) = nonNegativeInt(rng)
    val (d, r2) = doubleRNG(r)
    ((x, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r) = doubleRNG(rng)
    val (x, r2) = nonNegativeInt(r)
    ((d, x), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, r) = doubleRNG(rng)
    val (d2, r2) = doubleRNG(r)
    val (d3, r3) = doubleRNG(r2)
    ((d, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, rng: RNG, xs: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (xs, rng)
      } else {
        val (x, r) = rng.nextInt
        go(count - 1, r, x :: xs)
      }
    }
    go(count, rng, Nil)
  }

  type Rand[+A] = RNG => (A, RNG)

  val intRNG: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(x => x - x % 2)

  def doubleViaMap: Rand[Double] = map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r) = ra(rng)
    val (b, r2) = rb(r)
    (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(intRNG, doubleRNG)

  def randDoubleInt: Rand[(Double, Int)] = both(doubleRNG, intRNG)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def nonNegativeLessThan(n: Int): Rand[Int] = rng => {
    val (x, r) = intRNG(rng)
    val mod = x % n
    if (x + n - 1 - mod >= 0) {
      (mod, r)
    } else {
      nonNegativeLessThan(n)(r)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(x => {
      val mod = x % n
      if (x + n - 1 - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    })

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(144477)
//    (1 to 4).foreach(i => {
//      val (xs, r) = ints(i)(rng)
//      println(xs)
//      println(r)
//    })
    println(nonNegativeLessThan(Int.MaxValue / 2 + 14444)(rng))
  }
}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
//    for {
//      a <- this
//      b <- sb
//    } yield f(a, b)
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List[A]()))((a, acc) => a.map2(acc)(_ :: _))

//  type Rand[A] = State[RNG, A]

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def update(input: Input, machine: Machine): Machine = (input, machine) match {
    case (Coin, Machine(true, candies, coins)) if candies > 0 => Machine(locked = false, candies, coins + 1)
    case (Turn, Machine(false, candies, coins)) if candies > 0 => Machine(locked = true, candies - 1, coins)
    case _ => machine
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(input => modify(m => update(input, m)))).flatMap(_ => get.map(s => (s.candies, s.coins)))

  def main(args: Array[String]): Unit = {
//    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    val machine = Machine(true, 5, 10)
    val (candies, coins) = simulateMachine(inputs).run(machine)
    println(coins)
    println(candies)
  }
}
