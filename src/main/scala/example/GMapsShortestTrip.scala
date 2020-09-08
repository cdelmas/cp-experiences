package example

import oscar.cp._
import scalaj.http.Http

/**
  * <b>Custom Traveling Salesman Problem solving feat. GMaps</b><br>
  * <ol>
  *   <li>Calls <a href="https://developers.google.com/maps/documentation/distance-matrix/overview">GMaps Distance Matrix API</a></li>
  *   <li>Parses its return</li>
  *   <li>Searches for shortest trip to reach all locations and come back to 1st one</li>
  * </ol>
  *
  * Vaguely inspired by <a href=https://bitbucket.org/oscarlib/oscar/src/dev/oscar-cp-examples/src/main/scala/oscar/cp/examples/ATSP.scala>official example</a>
  */
object GMapsShortestTrip extends CPModel with App {

  // Locations to drive through (max 10, otherwise multiple API calls required)
  val locations = Array(
    "Reims",
    "Lyon",
    "Amiens",
    "Bourges",
    "Dijon",
    "Clermont-Ferrand",
    "Rouen",
    "Le Mans",
    "Troyes",
    "Tours, FRANCE"
  )

  // GMaps API call
  println("Enter a valid GMaps distancematrix API key and press [Enter] - https://developers.google.com/maps/documentation/distance-matrix/overview")
  val apiKey = scala.io.StdIn.readLine()
  val addresses = locations.mkString("|")
  val response = Http("https://maps.googleapis.com/maps/api/distancematrix/json")
    .params(Map(
      "origins" -> addresses,
      "destinations" -> addresses,
      "key" -> apiKey
    )).asString
  println(response.body)

  // API result parsing
  val json = ujson.read(response.body)
  val outputLocations = json("origin_addresses").arr
  val distMatrix = Array.ofDim[Int](outputLocations.size, outputLocations.size)
  val timeMatrix = Array.ofDim[Int](outputLocations.size, outputLocations.size)
  json("rows").arr.zipWithIndex.foreach { case(row,i) =>
    row("elements").arr.zipWithIndex.foreach{ case(el,j) =>
      distMatrix(i)(j) = el("distance").obj("value").num.toInt
      timeMatrix(i)(j) = el("duration").obj("value").num.toInt
    }
    println(outputLocations(i) + "\t" + distMatrix(i).mkString("\t"))
  }

  // Variable
  val trip = Array.fill(outputLocations.size)(CPIntVar(outputLocations.size))

  // Total distance & time calculation
  /**
    * From trip, sum matrix values
    * @param trip the trip with each step
    * @param matrix step diffs
    * @return sum
    */
  def calculateSum(trip: Array[CPIntVar], matrix: Array[Array[Int]]) = {
    // for each step,
    trip.zipWithIndex.map { case(step, idx) =>
      // get "distance" with next step (or first step for the last one, hence the modulo)
      matrix(step)(trip((idx+1) % trip.length)).value
    // then sum them all
    }.sum
  }
  val totDist = calculateSum(trip, distMatrix)
  val totTime = calculateSum(trip, timeMatrix)

  // constraints
  // 1. start with first location
  add(trip(0) === 0)
  // 2. only once through every location
  add(allDifferent(trip))

  // optimization : minimize distance (could use time instead here)
  minimize(totDist)

  // go !
  search {
    binaryFirstFail(trip)
  }

  // print solutions
  onSolution {
    println( totDist + "m traveled in " + totTime + "s : " + trip.map(idx => outputLocations(idx.value)).mkString(" -> "))
  }

  println(start())
}

