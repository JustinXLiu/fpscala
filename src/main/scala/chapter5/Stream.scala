package chapter5

sealed trait Stream[+A] {

  import Stream._

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => List.empty
  }

  def take(n: Int): Stream[A] = {
    if (n == 0) Empty else this match {
      case Cons(h, t) => cons(h(), t().take(n - 1))
      case Empty => Empty
    }
  }

  def drop(n: Int): Stream[A] = {
    def helper(rest: Stream[A], n: Int): Stream[A] = {
      if (n == 0) rest else rest match {
        case Cons(_, t) => helper(t(), n - 1)
        case Empty => Empty
      }
    }
    helper(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => cons(h(), t().takeWhile(p))
    case Empty => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile1(p: A => Boolean): Stream[A] = foldRight(Stream[A]())((h, t) => if(p(h)) cons(h, t) else Empty)

  def headOption: Option[A] = ???

  def map[B](f: A => B): Stream[B] = foldRight(Stream[B]())((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream[A]())((h, t) => if(f(h)) cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream[B]())((h, t) => f(h).append(t))

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def helper(a: Int, b: Int): Stream[Int] = cons(a, helper(b, a + b))
    helper(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Empty
      case Some(p) => cons(p._1, unfold(p._2)(f))
    }
  }

  def constant1[A](a: A): Stream[A] = unfold(true)(_ => Some((a, true)))

  def from1(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  def fibs1: Stream[Int] = unfold((0, 1))({
    case (a, b) => Some((a, (b, a + b))) // without case won't compile
  })

  def ones = unfold(true)(_ => Some((1, true)))

}

