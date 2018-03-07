package chapter6

case class State[S, +A](run: S => (A, S)) {

  //6.10
  def flatMap[B](g: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    g(a).run(s1)
  })

  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => rb.map(b => f(a, b)))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def get[S]: State[S, S] = State(s => (s, s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield()
}

object State {

  //6.10
  // Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def flatMap[S, A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = f.run(s)
    g(a).run(s1)
  })

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = flatMap(s)(a => unit(f(a)))

  def map2[S, A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    fs.foldRight[State[S, List[A]]](unit[S, List[A]](List[A]()))((a: State[S, A], result: State[S, List[A]]) => map2(a, result)((a: A, b: List[A]) => (a :: b)))
  }
}