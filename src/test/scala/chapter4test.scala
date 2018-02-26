package chapter4

import org.scalatest.FunSuite

class chapter4test extends FunSuite {

  val some1 = Some(1)
  val some2 = Some(2)
  val some3 = Some(3)
  val none = None

  val plus1: Int => Int = x => x + 1
  val plus1opt: Int => Option[Int] = x => Some(x + 1)
  val toNone: Int => Option[Int] = _ => None

  test("option map") {
    assert(some1.map(plus1) == some2)
    assert(none.map(plus1) == none)
  }

  test("option flatMap") {
    assert(some1.flatMap(plus1opt) == some2)
    assert(some1.flatMap(toNone) == none)
    assert(none.flatMap(plus1opt) == none)
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

  test("option sequence") {
    assert(Option.sequence(List(some1, some2, some3)) == Some(List(1, 2, 3)))
    assert(Option.sequence(List(some1, none, some3)) == None)
  }

  test("option traverse") {
    assert(Option.traverse[Int, Int](List(1, 2, 3))(plus1opt) == Some(List(2, 3, 4)))
    assert(Option.traverse[Int, Int](List(1, 2, 3))(toNone) == none)
  }

  val l = Left("Error")
  val r1 = Right(1)
  val r2 = Right(2)
  val r3 = Right(3)

  val check1: Int => Either[String, Int] = x => if(x == 1) r1 else l
  val sum: (Int, Int) => Int = (x, y) => x + y

  test("either map") {
    assert(r1.map(plus1) == r2)
    assert(l.map(plus1) == l)
  }

  test("either flatmap") {
    assert(l.flatMap(check1) == l)
    assert(r1.flatMap(check1) == r1)
    assert(r2.flatMap(check1) == l)
  }

  test("either orElse") {
    assert(r1.orElse(r2) == r1)
    assert(l.orElse(r2) == r2)
  }

  test("either map2") {
    assert(l.map2(r1)(sum) == l)
    assert(r1.map2(l)(sum) == l)
    assert(r1.map2(r2)(sum) == r3)
  }

  test("either sequence") {
    assert(Either.sequence(List(r1, r2, r3)) == Right(List(1, 2, 3)))
    assert(Either.sequence(List(l, r2, r3)) == l)
  }

  test("either traverse") {
    assert(Either.traverse[String, Int, Int](List(1, 2, 3))(check1) == l)
    assert(Either.traverse[String, Int, Int](List(1, 1, 1))(check1) == Right(List(1, 1, 1)))
  }
}

