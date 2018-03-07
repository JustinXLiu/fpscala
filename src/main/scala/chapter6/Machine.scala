package chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, coins: Int, candies: Int)

object Machine {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def helper(remainInputs: List[Input], s: State[Machine, (Int, Int)]): State[Machine, (Int, Int)] = remainInputs match {
      case Nil => s
      case h :: t => {
        s.modify((m: Machine) => (h, m) match {
          case (_, Machine(_, 0, _)) => m
          case (Coin, Machine(false, _, _)) => m
          case (Coin, Machine(true, x, y)) => Machine(false, x + 1, y)
          case (Turn, Machine(true, _, _)) => m
          case (Turn, Machine(false, x, y)) => Machine(true, x, y - 1)
        })
        helper(t, s)
      }
    }
    helper(inputs, State(m => ((m.coins, m.candies), m)))
  }
}

