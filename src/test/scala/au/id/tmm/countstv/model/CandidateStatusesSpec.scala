package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CandidateStatusesSpec extends ImprovedFlatSpec {

  private val testCandidateStatuses: CandidateStatuses[Fruit] = CandidateStatuses(
    Apple -> CandidateStatus.Elected(ordinalElected = 0, electedAtCount = 1),
    Banana -> CandidateStatus.Remaining,
    Pear -> CandidateStatus.Ineligible,
    Strawberry -> CandidateStatus.Excluded(ordinalExcluded = 0, excludedAtCount = 1),
  )

  "a collection of candidate statuses" should "indicate the status of each candidate" in {
    assert(testCandidateStatuses.asMap(Apple) === CandidateStatus.Elected(ordinalElected = 0, electedAtCount = 1))
    assert(testCandidateStatuses.asMap(Banana) === CandidateStatus.Remaining)
    assert(testCandidateStatuses.asMap(Pear) === CandidateStatus.Ineligible)
    assert(testCandidateStatuses.asMap(Strawberry) === CandidateStatus.Excluded(ordinalExcluded = 0, excludedAtCount = 1))
  }

  it should "have each candidate" in {
    assert(testCandidateStatuses.allCandidates === Set(Apple, Banana, Pear, Strawberry))
  }

  it should "indicate elected candidates" in {
    assert(testCandidateStatuses.electedCandidates === DupelessSeq(Apple))
  }

  it should "indicate elected candidates in order" in {
    val testCandidateStatuses: CandidateStatuses[Fruit] = CandidateStatuses(
      Apple -> CandidateStatus.Elected(ordinalElected = 2, electedAtCount = 1),
      Banana -> CandidateStatus.Elected(ordinalElected = 1, electedAtCount = 1),
      Pear -> CandidateStatus.Ineligible,
      Strawberry -> CandidateStatus.Excluded(ordinalExcluded = 0, excludedAtCount = 1),
    )

    assert(testCandidateStatuses.electedCandidates.toList === List(Banana, Apple))
  }

  it should "indicate remaining candidates" in {
    assert(testCandidateStatuses.remainingCandidates === Set(Banana))
  }

  it should "indicate ineligible candidates" in {
    assert(testCandidateStatuses.ineligibleCandidates === Set(Pear))
  }

  it should "indicate excluded candidates" in {
    assert(testCandidateStatuses.excludedCandidates === Set(Strawberry))
  }

  it should "indicate the candidates that are ineligble for preference flows" in {
    assert(testCandidateStatuses.ineligibleForPreferenceFlows === Set(Apple, Pear, Strawberry))
  }
}
