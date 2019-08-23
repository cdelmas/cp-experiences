package example

import oscar.cp._
import oscar.cp.scheduling.visual.VisualGanttChart
import oscar.util.RandomGenerator
import oscar.visual._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object TaskScheduler extends CPModel with App {
/**
  * Job-Shop Problem
  *  A Job is a a sequence of n Activities that must be executed one after the
  *  others. There are n machines and each activity of the jobs require one of the
  *  n machines. The objective is to assign the starting time of each activity
  *  minimizing the total makespan and such that no two activities from two different
  *  jobs requiring the same machine overlap.
  *
  *
  *  @author Pierre Schaus  pschaus@gmail.com
  *  @author Renaud Hartert ren.hartert@gmail.com
  *  @author Cyrille Dejemeppe cyrille.dejemeppe@gmail.com
  */

  // Parsing
  // -----------------------------------------------------------------------

  var lines = Source.fromResource("data/ft07.txt").getLines().toList

  val nJobs = lines.head.trim().split(" ")(0).toInt
  val nTasksPerJob = lines.head.trim().split(" ")(1).toInt
  val nResources = lines.head.trim().split(" ")(2).toInt

  val nActivities = nJobs * nTasksPerJob

  val Activities = 0 until nActivities
  val Jobs = 0 until nJobs
  val Resources = 0 until nResources

  lines = lines.drop(1)

  val jobs = Array.fill(nActivities)(0)
  val resources = Array.fill(nActivities)(0)
  val durations = Array.fill(nActivities)(0)

  for (i <- Activities) {

    val l = lines.head.trim().split("[ ,\t]+").map(_.toInt)

    jobs(i) = l(0)
    resources(i) = l(1)
    durations(i) = l(2)

    lines = lines.drop(1)
  }

  // Modeling
  // -----------------------------------------------------------------------
  val horizon = durations.sum

  // Activities & Resources
  val durationsVar = Array.tabulate(nActivities)(t => CPIntVar(durations(t)))
  val startsVar = Array.tabulate(nActivities)(t => CPIntVar(0 to horizon - durationsVar(t).min))
  val endsVar = Array.tabulate(nActivities)(t => startsVar(t)+durations(t))
  val demandsVar = Array.fill(nActivities)(CPIntVar(1))
  val resourcesVar = Array.tabulate(nActivities)(t => CPIntVar(resources(t)))

  val makespan = maximum(endsVar)

  val bestSolutionStarts = Array.ofDim[Int](nActivities)

  // Visualization
  // -----------------------------------------------------------------------

  val frame = new VisualFrame("JobShop Problem", 2, 1)
  val colors = VisualUtil.getRandomColors(nResources, pastel = true)
  val gantt1 = new VisualGanttChart(startsVar, durationsVar, endsVar, i => jobs(i), colors = i => colors(resources(i)))
  val gantt2 = new VisualGanttChart(startsVar, durationsVar, endsVar, i => resources(i), colors = i => colors(resources(i)))
  onSolution {
    for (a <- 0 until nActivities) {
      bestSolutionStarts(a) = startsVar(a).min
    }
    gantt1.update(1, 20)
    gantt2.update(1, 20)
  }
  frame.createFrame("Gantt chart").add(gantt1)
  frame.createFrame("Gantt chart").add(gantt2)
  frame.pack()

  // Constraints & Search
  // -----------------------------------------------------------------------

  // Precedences
  for (t <- 1 to Activities.max if jobs(t - 1) == jobs(t)) {
    add(endsVar(t - 1) <= startsVar(t))
  }
  // Cumulative
  val rankBranchings = for (r <- Resources) yield {
    def filter(x: Array[CPIntVar]) = Activities.filter(resources(_) == r).map(x(_))
    val (s,d,e) = (filter(startsVar), filter(durationsVar), filter(endsVar))
    add(unaryResource(s,d,e))
    rank(s,d,e)
  }

  minimize(makespan)

  val rankBranching = rankBranchings.reduce{_++_}

  search {
    rankBranchings.reduce{_++_} ++ binaryStatic(startsVar)
  }
  start(1)
  // LNS
  val constraintBuffer = ArrayBuffer[Constraint]()
  val maxFails = 2000
  val relaxProba = 90
  val nRelaxations = 100000
  for (r <- 1 to nRelaxations) {
    constraintBuffer.clear()
    val stats = startSubjectTo(failureLimit = maxFails) {
      for (a <- 0 until nActivities) {
        if (RandomGenerator.nextInt(100) > relaxProba) {
          constraintBuffer += startsVar(a) === bestSolutionStarts(a)
        }
      }
      add(constraintBuffer)
    }
  }
}

