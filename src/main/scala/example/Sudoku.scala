package example

import oscar.cp._

object Sudoku extends CPModel with App {

  val size = 9

  val sudokuRange = 1 to size

  type Matrix = Array[Array[CPIntVar]]

  var S: Matrix = createSudoku

  sudokuRange foreach { i =>
    add(allDifferent(row(S)(i - 1)))
    add(allDifferent(column(S)(i - 1)))
    add(allDifferent(zone(S)(i - 1)))
  }

  search {
    binary(S(0))
  } onSolution {
    displaySolution(S)
  }

  start()

  def row(a: Matrix)(r: Int): Array[CPIntVar] = a(r)

  def column(a: Matrix)(c: Int): Array[CPIntVar] = (for {
    i <- a.indices
    v = a(i)(c)
  } yield v).toArray

  def subMatrix(a: Matrix)(rowRange: Range)(columnRange: Range): Array[CPIntVar] = for {
    i <- rowRange
    j <- columnRange
  } yield a(i)(j)

  def zone(a: Matrix)(i: Int): Array[CPIntVar] = i match {
    case 0 => subMatrix(a)(0 to 2)(0 to 2)
    case 1 => subMatrix(a)(0 to 2)(3 to 5)
    case 2 => subMatrix(a)(0 to 2)(6 to 8)
    case 3 => subMatrix(a)(3 to 5)(0 to 2)
    case 4 => subMatrix(a)(3 to 5)(3 to 5)
    case 5 => subMatrix(a)(3 to 5)(6 to 8)
    case 6 => subMatrix(a)(6 to 8)(0 to 2)
    case 7 => subMatrix(a)(6 to 8)(3 to 5)
    case 8 => subMatrix(a)(6 to 8)(6 to 8)
  }

  def displaySolution(sol: Matrix): Unit = sol foreach { l =>
    l.foreach { v =>
      print(s"${v.value} ")
    }
    println()
  }

  def createSudoku: Matrix = {
    val m =  Array.tabulate(size, size)((_,_) => CPIntVar(sudokuRange))

    // example sudoku
    /*
     *       . . 4 1 5 . . 6 .
     *       . 8 . 4 . . . 2 5
     *       5 . . . . . . . .
     *       6 7 . . 8 . . . .
     *       9 . . 5 . 1 . . 3
     *       . . . . 9 . . 8 7
     *       . . . . . . . . 4
     *       8 3 . . . 6 . 1 .
     *       . 5 . . 1 8 3 . .
     *
     */
    // set constants = facts
    m(0)(2) = CPIntVar(4)
    m(0)(3) = CPIntVar(1)
    m(0)(4) = CPIntVar(5)
    m(0)(7) = CPIntVar(6)
    m(1)(1) = CPIntVar(8)
    m(1)(3) = CPIntVar(4)
    m(1)(7) = CPIntVar(2)
    m(1)(8) = CPIntVar(5)
    m(2)(0) = CPIntVar(5)
    m(3)(0) = CPIntVar(6)
    m(3)(1) = CPIntVar(7)
    m(3)(4) = CPIntVar(8)
    m(4)(0) = CPIntVar(9)
    m(4)(3) = CPIntVar(5)
    m(4)(5) = CPIntVar(1)
    m(4)(8) = CPIntVar(3)
    m(5)(4) = CPIntVar(9)
    m(5)(7) = CPIntVar(8)
    m(5)(8) = CPIntVar(7)
    m(6)(8) = CPIntVar(4)
    m(7)(0) = CPIntVar(8)
    m(7)(1) = CPIntVar(3)
    m(7)(5) = CPIntVar(6)
    m(7)(7) = CPIntVar(1)
    m(8)(1) = CPIntVar(5)
    m(8)(4) = CPIntVar(1)
    m(8)(5) = CPIntVar(8)
    m(8)(6) = CPIntVar(3)

    m
  }
}