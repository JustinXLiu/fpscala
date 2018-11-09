package chapter3

import org.scalatest.FunSuite

class chapter3test extends FunSuite {

  val l = List(1, 2, 3, 4, 5)
  val lessThan3: Int => Boolean = x => x < 3

  test("tail") {
    assert(List.tail(Nil) == Nil)
    assert(List.tail(l) == List(2, 3, 4, 5))
  }

  test("set Head") {
    assert(List.setHead(l, 10) == List(10, 2, 3, 4, 5))
    assert(List.setHead(Nil, 10) == Nil)
  }

  test("drop") {
    assert(List.drop(l, 0) == l)
    assert(List.drop(l, 3) == List(4, 5))
  }

  test("dropWhile") {
    assert(List.dropWhile(Nil, lessThan3) == Nil)
    assert(List.dropWhile(l, lessThan3) == List(3, 4, 5))
  }

  test("init") {
    assert(List.init(Nil) == Nil)
    assert(List.init(l) == List(1, 2, 3, 4))
  }

  test("length") {
    assert(List.length(Nil) == 0)
    assert(List.length(l) == 5)
  }

  test("foldLeft op") {
    assert(List.sumWithFL(Nil) == 0)
    assert(List.productWithFL(Nil) == 1)
    assert(List.lengthWithFL(Nil)== 0)

    assert(List.sumWithFL(l) == 15)
    assert(List.productWithFL(l) == 120)
    assert(List.lengthWithFL(l)== 5)
  }

  test("reverse") {
    assert(List.reverse(Nil) == Nil)
    assert(List.reverse(l) == List(5, 4, 3, 2, 1))
  }

  test("append") {
    assert(List.append(l, List(6, 7)) == List(1, 2, 3, 4, 5, 6, 7))
  }

  test("concat") {
    assert(List.concat(List(l, l)) == List(1, 2, 3, 4, 5, 1, 2, 3, 4, 5))
  }










  test("flatMap") {
    assert(List.flatMap(l)(i => List(i, i)) == List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
  }

  test("filter with flatMap") {
    assert(List.filterWithFlatMap(l)(lessThan3) == List(1, 2))
  }

}
