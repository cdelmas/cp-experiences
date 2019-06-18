package example

import oscar.cp._

object Reified extends CPModel with App {

  val X = Array.fill(5)(CPIntVar(0 to 5))
  val count = sum(0 until 5)(i => (X(i) ?>= 2) && (X(i) ?< 4))
  // count the number of variables taking a value between 2 and 3 (inclusive)
}
