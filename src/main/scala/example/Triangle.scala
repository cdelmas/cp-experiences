package example

import oscar.cp._

object Triangle extends CPModel with App {

  val max = 200

  val X = CPIntVar(1 to max)
  val Y = CPIntVar(1 to max)

  val H = CPIntVar(1 to max)

  add(X * X + Y * Y === H * H)

  // maximize(X * Y) // -> step 2

  search {
    binaryFirstFail(Seq(X, Y, H))
  } onSolution {
    println(
      s"Solution found, value of (X,Y,H) in this solution: + (${X.value},${Y.value},${H.value}), area = ${(X.value * Y.value) / 2}"
    )
  }

  start(nSols = 10)
}
