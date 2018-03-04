package chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object chapter6 {

  //6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng_) = rng.nextInt
    // -Int.MinValue == Int.MinValue hence plus 1
    (if (n < 0) -(n + 1) else n, rng_)
  }

  //6.2
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng_) = nonNegativeInt(rng)
    ((n / (1 + Int.MaxValue)).toDouble, rng_)
  }

  //6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng_) = rng.nextInt
    val (d, rng2_) = double(rng_)
    ((i, d), rng2_)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng_) = intDouble(rng)
    ((d, i), rng_)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng_) = double(rng)
    val (d1, rng2_) = double(rng_)
    val (d2, rng3_) = double(rng2_)
    ((d, d1, d2), rng3_)
  }

  //6.4
  def ints(count: Int)(rng: RNG) : (List[Int], RNG) ={
    def helper(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (c == count) (acc, r)
      else {
        val (i, r_) = r.nextInt
        helper(c + 1, r_, i :: acc)
      }
    }
    helper(0, rng, List())
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, r) = s(rng)
      (f(a), r)
  }

  //6.5
  def double1: Rand[Double] = map(nonNegativeInt)(i => (i / (1 + Int.MaxValue)).toDouble)

  //6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = r => {
    val (a, r1) = ra(r)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((a, b) => (a, b))

  //6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  def ints1(count: Int): Rand[List[Int]] = ???

  //6.8


  //6.9

}