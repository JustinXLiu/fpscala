package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  //3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  //3.26
  def maximun(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximun(l).max(maximun(r))
  }

  //3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => depth(l).max(depth(r)) + 1
  }

  //3.28
  def map[A, S](tree: Tree[A])(f: A => S): Tree[S] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //3.29*
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeWithFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((l, r) => 1 + l + r)

  // can't use _ => _ here for f
  def maximumWithFold(tree: Tree[Int]): Int = fold(tree)(a => a)((l, r) => l.max(r))

  def depthWithFold[A](tree: Tree[A]): Int = fold(tree)(a => 0)((l, r) => l.max(r) + 1)

  def mapWithFold[A, S](tree: Tree[A])(f: A => S): Tree[S] = fold(tree)(a => Leaf(f(a)): Tree[S])((l, r) => Branch(l, r))
}
