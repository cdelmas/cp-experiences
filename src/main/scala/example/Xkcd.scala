package example
import oscar.cp._

/**
  *
  * Implementing Xkcd knapsack problem in Oscar
  *
  * http://xkcd.com/287/
  *
  * Some amount (or none) of each dish should be ordered to
  * give a total of exact 15.05
  *
  *
  * @author Hakan Kjellerstrand hakank@gmail.com
  * http://www.hakank.org/oscar/
  *
  */
object Xkcd extends CPModel with App {
  // data
  val price = Array(215, 275, 335, 355, 420, 580) // in cents
  val num_prices = price.length
  val total = 1505
  // variables
  val x = Array.fill(num_prices)(CPIntVar(0 to 10))
  //
  // constraints
  //
  var numSols = 0

  add(weightedSum(price, x) === total)
  search {
    binaryFirstFail(x)
  }
  onSolution {
    println("x:" + x.mkString(" "))
    numSols += 1
  }
  println(start())
}
