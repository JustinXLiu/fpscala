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
        h match {
          case Coin => {
            val transform: Machine => Machine = m => m match {
              case Machine(a, 0, b) => Machine(a, 0, b)
              case Machine(false, x, y) => Machine(false, x, y)
              case Machine(true, x, y) if y > 0 => Machine(true, x + 1, y)
            }
            s.modify(transform)
            helper(t, s)
          }
          case Turn => {
            val transform: Machine => Machine = m => m match {
              case Machine(a, 0, b) => Machine(a, 0, b)
              case Machine(true, x, y) => Machine(true, x, y)
              case Machine(false, x, y) if y > 0 => Machine(true, x, y - 1)
            }
            s.modify(transform)
            helper(t, s)
          }
        }
      }
    }
    helper(inputs, State(m => ((m.coins, m.candies), m)))
  }
}

