package au.id.tmm.countstv.performancetest

import au.id.tmm.countstv.counting.FullCountComputation
import au.id.tmm.countstv.model.{CandidateStatuses, PreferenceTree}
import org.kohsuke.randname.RandomNameGenerator
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Random

object Runner {

  private val logger: Logger = LoggerFactory.getLogger(Runner.getClass.getName.stripSuffix("$"))

  def runCountFor(
                   numCandidates: Int,
                   numIneligible: Int,
                   numVacancies: Int,
                   numBallots: Int,
                 ): CandidateStatuses[Candidate] = {
    require(numIneligible < numCandidates)

    logger.info("Generating candidates")
    val candidates = generateCandidates(numCandidates)

    val ineligibleCandidates = candidates.take(numIneligible)

    val ballots = generateBallots(candidates, numBallots)

    logger.info("Building preference tree")
    val preferenceTree = PreferenceTree.from(ballots)

    logger.info("Running count")
    FullCountComputation.runCount(candidates, ineligibleCandidates, numVacancies, preferenceTree)
      .anyOutcome
      .last
      .candidateStatuses
  }

  private def generateCandidates(numCandidates: Int): Set[Candidate] = {
    val randomNameGenerator = new RandomNameGenerator()

    (0 until numCandidates)
      .map(Candidate(_, randomNameGenerator.next()))
      .toSet
  }

  private def generateBallots(candidates: Set[Candidate], numBallots: Int): Iterable[Vector[Candidate]] = {
    val numCandidates = candidates.size
    val candidatesAsVector = candidates.toVector

    (0 until numBallots)
      .toStream
      .map { _ =>
        val numPreferences = Random.nextInt(numCandidates) + 1

        Random.shuffle(candidatesAsVector).take(numPreferences)
      }
  }

  final case class Candidate(id: Int, name: String)

}
