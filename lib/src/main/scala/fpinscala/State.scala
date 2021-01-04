package fpinscala.state

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

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(2)
    (1 to 4).foreach(i => {
      val (xs, r) = ints(i)(rng)
      println(xs)
      println(r)
    })
  }
}
