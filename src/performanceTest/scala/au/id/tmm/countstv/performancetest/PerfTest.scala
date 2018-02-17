package au.id.tmm.countstv.performancetest

import au.id.tmm.countstv.model.CandidateStatuses
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class PerfTest extends ImprovedFlatSpec {

  "a big count" should "succeed" in {
    val result = Runner.runCountFor(
      numCandidates = 30,
      numIneligible = 3,
      numVacancies = 12,
      numBallots = 10000,
    )

    println(prettyResults(result))
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
