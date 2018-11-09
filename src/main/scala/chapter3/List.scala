package chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  //3.2
  def tail[A](input: List[A]): List[A] = input match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  //3.3
  def setHead[A](input: List[A], value: A): List[A] = input match {
    case Nil => Nil
    case Cons(_, tail) => Cons(value, tail)
  }

  //3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n - 1)
    }
  }

  //3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => l
  }

  //3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  //not stack safe
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  //3.7
  //not possible

  //3.8
  //get back same list

  //3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, sum) => sum + 1)

  //3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  //3.11
  def sumWithFL(l: List[Int]) = foldLeft(l, 0)(_ + _)
  def productWithFL(l: List[Int]) = foldLeft(l, 1)(_ * _)
  def lengthWithFL[A](l: List[A]) = foldLeft(l, 0)((sum, _) => sum + 1)

  //3.12
  def reverse[A](l: List[A]) = foldLeft(l, List[A]())((acc, head) => Cons(head, acc))

  //3.13
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  //3.14 *
  def append[A](a: List[A], b: List[A]): List[A] = foldRight2(a, b)(Cons(_, _))

  //3.15
  def concat[A](a: List[List[A]]): List[A] = foldLeft(a, List[A]())(append)

  //3.16
  def addOne(a: List[Int]): List[Int] = foldRight2(a, Nil: List[Int])((h, t) => Cons(h + 1, t))

  //3.17
  def toListString(a: List[Double]): List[String] = foldRight2(a, Nil: List[String])((h, t) => Cons(h.toString, t))

  //3.18
  def map[A, S](as: List[A])(f: A => S): List[S] = foldRight2(as, Nil: List[S])((h, t) => Cons(f(h), t))

  //3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight2(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  //3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight2(as, Nil: List[B])((h, t) => append(f(h), t))

  //3.21
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

  //3.22
  def addPair(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h1, t1)) => Cons(h + h1, addPair(t, t1))
  }

  //3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h, t), Cons(h1, t1)) => Cons(f(h, h1), zipWith(t, t1)(f))
  }

  //3.24
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = ???

}