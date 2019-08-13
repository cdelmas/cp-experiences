package example

import oscar.cp._

object Dinner extends CPModel with App {

  // 20 people, represented by their topic list
  val peopleList = Array(
    Set(1, 0, 7, 8, 5), // 0
    Set(0, 3, 7, 4, 9),
    Set(1, 2, 3, 4, 5),
    Set(6, 7, 8, 9, 0),
    Set(1, 2, 6, 3, 4),
    Set(4, 6, 8, 0, 2), // 5
    Set(1, 3, 5, 7, 9),
    Set(1, 4, 7, 9, 2),
    Set(0, 3, 6, 9, 2),
    Set(2, 4, 5, 6, 7),
    Set(0, 1, 2, 3, 8), // 10
    Set(4, 5, 1, 2, 3),
    Set(9, 8, 3, 2, 1),
    Set(2, 3, 4, 5, 8),
    Set(7, 3, 4, 1, 2),
    Set(7, 8, 9, 0, 2), // 15
    Set(1, 5, 0, 9, 2),
    Set(0, 1, 9, 8, 3),
    Set(0, 3, 2, 1, 7),
    Set(7, 6, 8, 2, 4)
  )

  val affinities = (for {
    i <- peopleList.indices
    j <- peopleList.indices
  } yield CPIntVar(peopleList(i).diff(peopleList(j)).size))
    .grouped(peopleList.length).toArray

  // 4 tables of 5 people
  val tableList = Array(
    0.to(4).indices.map(_ => CPIntVar(0 until 20)).toArray,
    0.to(4).indices.map(_ => CPIntVar(0 until 20)).toArray,
    0.to(4).indices.map(_ => CPIntVar(0 until 20)).toArray,
    0.to(4).indices.map(_ => CPIntVar(0 until 20)).toArray
  )

  // rule #1: each table have 5 (different) people
  tableList foreach { table =>
    add(allDifferent(table))
  }

  // rule #2: people cannot join more than one table
  add(allDifferent(tableList.flatten))

  // rule #3: adjacent people have two favourite topics in common
  tableList foreach { table =>
    for(i <- table.indices) {
      add(affinities(i)(i+1) >= 2)
    }
  }

  search {
    binaryStatic(tableList.flatten[CPIntVar])
  }

  onSolution {
    println(tableList.foldLeft("Tables: ")((acc, table) => acc + table.foldLeft("Table: ")((tbl, person) => s"$tbl - ${person.value} ")))
  }

  start(timeLimit = 10)
}
