package example

import oscar.cp._

// the logical or “||”, the logical and “&&” and the implication “==>”.
object Logical extends CPModel with App {

  val A = CPBoolVar()
  val B = CPBoolVar()
  val C = CPBoolVar()
  val D = CPBoolVar()

  add(A && B) // is equivalent to add((A && B) == 1) which is equivalent to add((A && B).constraintTrue())

  add(((A ==> B) || C) && D)

  search {
    binaryStatic(Seq(A, B, C, D))
  } onSolution {
    println(A + " " + B + " " + C + " " + D)
  } start ()

}
