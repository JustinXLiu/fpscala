package chapter4

import org.scalatest.FunSuite

class chapter4test extends FunSuite {

  val some1 = Some(1)
  val some2 = Some(2)
  val none = None

  val intToIntPlusOne: Int => Int = x => x + 1
  val intToOptPlusOne: Int => Option[Int] = x => Some(x + 1)
  val intToOptNone: Int => Option[Int] = _ => None

  val random: Any => Int = _ => 1
  val randomOpt: Any => Option[Int] = _ => Some(1)

  test("option map") {
    assert(some1.map(intToIntPlusOne) == some2)
    assert(none.map(random) == none)
  }

  test("option flatMap") {
    assert(some1.flatMap(intToOptPlusOne) == some2)
    assert(some1.flatMap(intToOptNone) == none)
    assert(none.flatMap(randomOpt) == none)
  }

  test("option getOrElse") {
    assert(some1.getOrElse(2) == 1)
    assert(none.getOrElse(2) == 2)
  }

  test("option orElse") {
    assert(some1.orElse(some2) == some1)
    assert(none.orElse(some2) == some2)
  }

  test("option filter") {
    assert(some1.filter(_ == 1) == some1)
    assert(some1.filter(_ == 2) == none)
    assert(some2.filter(_ == 2) == some2)
    assert(some1.filter(_ != 1) == none)
  }

  import chapter4._

  test("option sequence") {
    assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
    assert(sequence(List(Some(1), None, Some(3))) == None)
  }

  test("option traverse") {
    assert(traverse[Int, Int](List(1, 2, 3))(intToOptPlusOne) == Some(List(2, 3, 4)))

  }
}

