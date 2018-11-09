package chapter2

import org.scalatest.FunSuite

class chapter2test extends FunSuite {

  test("fib") {
    assert(Chapter2.fib(0) == 0)
    assert(Chapter2.fib(1) == 1)
    assert(Chapter2.fib(2) == 1)
    assert(Chapter2.fib(3) == 2)
    assert(Chapter2.fib(4) == 3)
    assert(Chapter2.fib(5) == 5)
  }

  test("array sorted") {
    val arr = Array(1, 3, 5, 7)
    val ascending: (Int, Int) => Boolean = (x, y) => x < y
    val diffentBy2: (Int, Int) => Boolean = (x, y) => y - x == 2

    assert(Chapter2.isSorted(arr, ascending) == true)
    assert(Chapter2.isSorted(arr, diffentBy2) == true)
  }
}
