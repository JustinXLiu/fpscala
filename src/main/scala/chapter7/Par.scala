package chapter7

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

  //7.3
  def map2Future[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
  //7.4
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    override def call: A = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
}
