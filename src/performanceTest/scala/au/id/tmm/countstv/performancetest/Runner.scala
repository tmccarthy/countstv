package au.id.tmm.countstv.performancetest

import au.id.tmm.countstv.counting.FullCountComputation
import au.id.tmm.countstv.model.{CandidateStatuses, PreferenceTree}
import au.id.tmm.utilities.logging.{LoggedEvent, Logger}
import org.kohsuke.randname.RandomNameGenerator

import scala.util.Random

object Runner {

  private implicit val logger: Logger = Logger()

  private val seed: Long = -42

  def runCountFor(
                   numCandidates: Int,
                   numIneligible: Int,
                   numVacancies: Int,
                   numBallots: Int,
                   numTimesToRunCount: Int = 1,
                 ): Option[CandidateStatuses[Candidate]] = {
    require(numIneligible < numCandidates)

    val candidates =
      LoggedEvent("GENERATING_CANDIDATES", "numCandidates" -> numCandidates)
        .logWithTimeOnceFinished {
          generateCandidates(numCandidates)
        }

    val ineligibleCandidates = candidates.take(numIneligible)

    val ballots = generateBallots(candidates, numBallots)

    val preferenceTree =
      LoggedEvent("GENERATING_PREFERENCE_TREE", "numBallots" -> numBallots)
        .logWithTimeOnceFinished {
          PreferenceTree.from(ballots)
        }

    System.gc()

    logger.info("STARTED_COUNT")

    (1 to numTimesToRunCount).foldLeft(None: Option[CandidateStatuses[Candidate]]) { (_, _) =>
      Some(
        LoggedEvent("RUN_COUNT")
        .logWithTimeOnceFinished {
          FullCountComputation.runCount(candidates, ineligibleCandidates, numVacancies, preferenceTree)
            .onlyOutcome
            .countSteps
            .last
            .candidateStatuses
        }
      )
    }
  }

  private def generateCandidates(numCandidates: Int): Set[Candidate] = {
    val randomNameGenerator = new RandomNameGenerator(seed.toInt)

    (0 until numCandidates)
      .map(Candidate(_, randomNameGenerator.next()))
      .toSet
  }

  private def generateBallots(candidates: Set[Candidate], numBallots: Int): Iterable[Vector[Candidate]] = {
    val random = new Random(seed)
    val numCandidates = candidates.size
    val candidatesAsVector = candidates.toVector

    (0 until numBallots)
      .toStream
      .map { _ =>
        val numPreferences = random.nextInt(numCandidates) + 1

        random.shuffle(candidatesAsVector).take(numPreferences)
      }
  }

  final case class Candidate(id: Int, name: String) {
    @inline override def hashCode(): Int = id
  }

}
