package chapter10

trait Monoid[A] {
  def op(a: A, b: A): A
  def zero: A
}

object Monoid {

  //10.1
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a + b
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a: Int, b: Int): Int = a * b
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a || b
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a: Boolean, b: Boolean): Boolean = a && b
    override def zero: Boolean = true
  }

  //10.2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a: Option[A], b: Option[A]): Option[A] = a orElse b
    override def zero: Option[A] = None
  }

  def optionMonoidOpp[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a: Option[A], b: Option[A]): Option[A] = b orElse a
    override def zero: Option[A] = None
  }

  //10.3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a: A => A, b: A => A): A => A = a.compose(b)
    override def zero: A => A = a => a
  }

  def endoMonoidOpp[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a: A => A, b: A => A): A => A = a.andThen(b)
    override def zero: A => A = a => a
  }

  //10.4

  //10.5
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldLeft(m.zero)(m.op)
  def foldMap1[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  //10.6
  def foldLeft[A, B](as: List[A])(b: B)(f: (B, A) => B): B = ???
  def foldRight[A, B](as: List[A])(b: B)(f: (A, B) => B): B = ???

  //10.7
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
    case 0 => m.zero
    case 1 => f(v(0))
    case _ => {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  //10.8

  //10.9
  def ordered(as: IndexedSeq[Int]): Boolean = ???


  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  //10.10
  val wcMonid: Monoid[WC] = new Monoid[WC] {
    override def op(a: WC, b: WC): WC = (a, b) match {
      case (Stub(a_), Stub(b_)) => Stub(a_ + b_)
      case (Stub(a_), Part(l, w, r)) => Part(a_ + l, w, r)
      case (Part(l, w, r), Stub(b_)) => Part(l, w, r + b_)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if (r1 + l2 == "") 0 else 1), r2)
    }
    override def zero: WC = Stub("")
  }

  //10.11
  def countWords(s: String): Int = ???



}