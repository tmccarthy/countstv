package au.id.tmm.countstv.performancetest

import au.id.tmm.countstv.model.CandidateStatuses
import au.id.tmm.utilities.testing.ImprovedFlatSpec

import scala.sys.SystemProperties

class PerfTest extends ImprovedFlatSpec {

  "a big count" should "succeed" in {

    println("Waiting 5 seconds before starting")
    Thread.sleep(5000)
    println("Starting...")

    val systemProperties = new SystemProperties()

    val result = Runner.runCountFor(
      numCandidates = systemProperties.getOrElse("numCandidates", "30").toInt,
      numIneligible = systemProperties.getOrElse("numIneligible", "3").toInt,
      numVacancies = systemProperties.getOrElse("numVacancies", "12").toInt,
      numBallots = systemProperties.getOrElse("numBallots", "10000").toInt,
      numTimesToRunCount = systemProperties.getOrElse("numTimesToRunCount", "2").toInt,
    )

    result.map(prettyResults).foreach(println)
  }

  private def prettyResults(results: CandidateStatuses[Runner.Candidate]): String = {
    results
      .asMap
      .toList
      .sortBy { case (candidate, _) => candidate.id }
      .map { case (candidate, result) => String.format("%-40s -> %s", candidate, result) }
      .mkString("\n")
  }

}
