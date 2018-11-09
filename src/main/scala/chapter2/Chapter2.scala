package chapter2

import scala.annotation.tailrec

object Chapter2 {

  //2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(i: Int, prev: Int = 0, next: Int = 1): Int = i match {
      case 0 => prev
      case 1 => next
      case _ => go(i - 1, next, prev + next)
    }
    go(n)
  }

  //2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean = {
      if (i >= as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else go(i + 1)
    }
    go(0)
  }

  //2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => (b => f(a, b))

  //2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  //2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}
