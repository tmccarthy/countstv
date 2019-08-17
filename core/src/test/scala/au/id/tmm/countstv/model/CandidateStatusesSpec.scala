package au.id.tmm.countstv.model

import au.id.tmm.countstv.Fruit
import au.id.tmm.countstv.Fruit._
import au.id.tmm.countstv.model.CandidateStatus._
import au.id.tmm.countstv.model.values.{Count, Ordinal}
import au.id.tmm.utilities.collection.DupelessSeq
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class CandidateStatusesSpec extends ImprovedFlatSpec {

  private val testCandidateStatuses: CandidateStatuses[Fruit] = CandidateStatuses(
    Apple -> Elected(Ordinal.first, electedAtCount = Count(1)),
    Banana -> Remaining,
    Pear -> Ineligible,
    Strawberry -> Excluded(Ordinal.first, excludedAtCount = Count(1)),
  )

  "a collection of candidate statuses" should "indicate the status of each candidate" in {
    assert(testCandidateStatuses.asMap(Apple) === Elected(Ordinal.first, electedAtCount = Count(1)))
    assert(testCandidateStatuses.asMap(Banana) === Remaining)
    assert(testCandidateStatuses.asMap(Pear) === Ineligible)
    assert(testCandidateStatuses.asMap(Strawberry) === Excluded(Ordinal.first, excludedAtCount = Count(1)))
  }

  it should "have each candidate" in {
    assert(testCandidateStatuses.allCandidates === Set(Apple, Banana, Pear, Strawberry))
  }

  it should "indicate elected candidates" in {
    assert(testCandidateStatuses.electedCandidates === DupelessSeq(Apple))
  }

  it should "indicate elected candidates in order" in {
    val testCandidateStatuses: CandidateStatuses[Fruit] = CandidateStatuses(
      Apple -> Elected(Ordinal.third, electedAtCount = Count(1)),
      Banana -> Elected(Ordinal.second, electedAtCount = Count(1)),
      Pear -> Ineligible,
      Strawberry -> Excluded(Ordinal.first, excludedAtCount = Count(1)),
    )

    assert(testCandidateStatuses.electedCandidates === DupelessSeq(Banana, Apple))
  }

  it should "indicate remaining candidates" in {
    assert(testCandidateStatuses.remainingCandidates === Set(Banana))
  }

  it should "indicate ineligible candidates" in {
    assert(testCandidateStatuses.ineligibleCandidates === Set(Pear))
  }

  it should "indicate excluded candidates" in {
    assert(testCandidateStatuses.excludedCandidates === DupelessSeq(Strawberry))
  }

  it should "indicate excluded candidates in order" in {
    val testCandidateStatuses: CandidateStatuses[Fruit] = CandidateStatuses(
      Apple -> Excluded(Ordinal.third, excludedAtCount = Count(3)),
      Banana -> Excluded(Ordinal.second, excludedAtCount = Count(2)),
      Pear -> Ineligible,
      Strawberry -> Excluded(Ordinal.first, excludedAtCount = Count(1)),
    )

    assert(testCandidateStatuses.excludedCandidates === DupelessSeq(Strawberry, Banana, Apple))
  }

  it should "indicate the candidates that are ineligible for preference flows" in {
    assert(testCandidateStatuses.ineligibleForPreferenceFlows === Set(Apple, Pear, Strawberry))
  }

  it should "indicate eligible candidates" in {
    assert(testCandidateStatuses.eligibleCandidates === Set(Apple, Banana, Strawberry))
  }

  it can "update the status of a single candidate" in {
    val actualCandidateStatuses = testCandidateStatuses.update(Banana, Ineligible)

    val expectedCandidateStatuses = CandidateStatuses(
      Apple -> Elected(Ordinal.first, electedAtCount = Count(1)),
      Banana -> Ineligible,
      Pear -> Ineligible,
      Strawberry -> Excluded(Ordinal.first, excludedAtCount = Count(1)),
    )

    assert(actualCandidateStatuses === expectedCandidateStatuses)
  }

  it can "update the statuses of many candidates" in {
    val actualCandidateStatuses = testCandidateStatuses.updateFrom(
      Map(
        Banana -> Excluded(Ordinal.second, excludedAtCount = Count(2)),
        Pear -> Elected(Ordinal.second, electedAtCount = Count(3)),
      )
    )

    val expectedCandidateStatuses = CandidateStatuses(
      Apple -> Elected(Ordinal.first, electedAtCount = Count(1)),
      Banana -> Excluded(Ordinal.second, excludedAtCount = Count(2)),
      Pear -> Elected(Ordinal.second, electedAtCount = Count(3)),
      Strawberry -> Excluded(Ordinal.first, excludedAtCount = Count(1)),
    )

    assert(actualCandidateStatuses === expectedCandidateStatuses)
  }

  it can "compute the difference to another CandidateStatuses object" in {
    val newCandidateStatuses: CandidateStatuses[Fruit] = CandidateStatuses(
      Apple -> Elected(Ordinal.first, electedAtCount = Count(1)),
      Banana -> Elected(Ordinal.second, electedAtCount = Count(2)),
      Pear -> Ineligible,
      Strawberry -> Excluded(Ordinal.first, excludedAtCount = Count(1)),
    )

    val expectedDiff = Map(Banana -> Elected(Ordinal.second, electedAtCount = Count(2)))

    assert((newCandidateStatuses diff testCandidateStatuses) === expectedDiff)
  }
}
