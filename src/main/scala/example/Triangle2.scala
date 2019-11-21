package example

import oscar.cp._

object Triangle2 extends CPModel with App {

  val max = 200

  val a = CPIntVar(1 to max)
  val b = CPIntVar(1 to max)

  val c = CPIntVar(1 to max)

  add(a * a + b * b === c * c)
  add(a + b + c < 200)

  val area = a * b // a * b / 2, x/2 est une fonction strictement croissante, donc inutile pour comparer ;)
  maximize(area)

  search {
    binarySplit(Seq(a,b,c))
  } onSolution {
    println(
      s"Solution found, value of (X,Y,H) in this solution: + (${a.value},${b.value},${c.value}), area = ${(a.value * b.value) / 2}"
    )
  }

  val stats = start()
  println(stats)
}
