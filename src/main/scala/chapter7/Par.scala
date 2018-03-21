package chapter7

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.TimeUnit

abstract class ExecutorService {
  def submit[A](a: Callable[A]): Future[A]
}
trait Callable[A] { def call: A }
trait Future[A] {
  def get: A
  def get(timeout: Long, unit: TimeUnit): A
  def cancel(evenIfRunning: Boolean): Boolean
  def isDone: Boolean
  def isCancelled: Boolean
}

object Par {

  type Par[A] = ExecutorService => Future[A]
  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  private case class Map2FutureWithTimeout[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    override def get: C = f(a.get, b.get)
    override def get(timeout: Long, unit: TimeUnit): C = ???
    override def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    override def isDone: Boolean = a.isDone && b.isDone
    override def isCancelled: Boolean = a.isCancelled || b.isCancelled
  }

  //7.3
  def map2Future[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    Map2FutureWithTimeout(af, bf, f)
  }

  //7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call: A = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit())((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  //7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))
  }

  //7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val l = as.map(asyncF((a: A) => if (f(a)) List(a) else Nil))
    map(sequence(l))(_.flatten)
  }
}
