package example

import oscar.cp._

object Triangle extends CPModel with App {

  val max = 198 // pourquoi 198??

  val a = CPIntVar(1 to max)
  val b = CPIntVar(1 to max)

  val c = CPIntVar(1 to max)

  add(a * a + b * b === c * c)
  add(a + b + c < 200)

  search {
    binaryLastConflict(Array(a, b, c))
  } onSolution {
    println(
      s"Solution found, value of (X,Y,H) in this solution: + (${a.value},${b.value},${c.value})}"
    )
  }

  val stats = start()
  println(stats)
}
