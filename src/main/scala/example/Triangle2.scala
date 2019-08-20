package example

import oscar.cp._

object Triangle2 extends CPModel with App {

  val max = 200

  val X = CPIntVar(1 to max)
  val Y = CPIntVar(1 to max)

  val H = CPIntVar(1 to max)

  add(X * X + Y * Y === H * H)
  add(X + Y + H < 200)

  val A = X * Y
  maximize(A)

  search {
    binarySplit(Seq(X,Y,H))
  } onSolution {
    println(
      s"Solution found, value of (X,Y,H) in this solution: + (${X.value},${Y.value},${H.value}), area = ${(X.value * Y.value) / 2}"
    )
  }

  val stats = start()
  println(stats)
}
