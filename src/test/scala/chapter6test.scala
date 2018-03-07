package chapter6

import chapter6._
import org.scalatest.FunSuite

class chapter6test extends FunSuite {

  val rng = SimpleRNG(42L)

  test("ints") {
    val (a, r_) = rng.nextInt
    val (b, r2_) = r_.nextInt
    val (c, r3_) = r2_.nextInt
    assert(ints(3)(rng) == (List(a, b, c), r3_))
    assert(ints(0)(rng) == (List(), rng))
    assert(ints(-1)(rng) == (List(), rng))
  }

  test("double with map") {
    assert(double1(rng) == double(rng))
  }

  test("map2") {
    val (a, r_) = double1(rng)
    val (b, r2_) = double1(r_)
    assert(map2(double1, double1)((x, y) => x + y)(rng) == (a + b, r2_))
  }

  test("sequence") {
    val (a, r_) = double1(rng)
    val (b, r2_) = double1(r_)
    val (c, r3_) = double1(r2_)
    assert(sequence(List(double1, double1, double1))(rng) == (List(a, b, c), r3_))
  }

  test("int with sequence") {
    assert(ints(5)(rng) == ints1(5)(rng))
  }

  test("flatMap") {
    val (d_, r_) = double(rng)
    val firstDigit: Double => Int = x => (x * 10).toInt
    assert(flatMap(double)(d => ints1(firstDigit(d)))(rng) == ints1(firstDigit(d_))(r_))
  }

  test("map with flatMap") {
    val times2: Double => Double = d => d * 2
    assert(map(double)(times2)(rng) == mapWithFlatMap(double)(times2)(rng))
  }

  test("map2 with flatMap") {
    val sum: (Double, Int) => Double = (x, y) => x + y
    assert(map2(double1, nonNegativeInt)(sum)(rng) == map2WithFlatMap(double1, nonNegativeInt)(sum)(rng))
  }

  test("state map") {
    val times2: Double => Double = d => d * 2
    assert(map(double)(times2)(rng) == State(double).map(times2).run(rng))
  }

  import Machine._
  test("simulate candy machine") {
    val machine = Machine(true, 5, 10)
    val inputs = List(Coin, Turn)
    assert(simulateMachine(inputs).run(machine) == ((6, 9), Machine(true, 6, 9)))
  }
}