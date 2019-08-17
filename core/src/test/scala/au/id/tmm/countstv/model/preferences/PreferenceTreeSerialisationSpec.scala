package au.id.tmm.countstv.model.preferences

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import au.id.tmm.countstv.counting.fixtures.CountFixture
import au.id.tmm.utilities.testing.ImprovedFlatSpec

class PreferenceTreeSerialisationSpec extends ImprovedFlatSpec {

  "a preference tree" can "be serialised and deserialised" in {
    val fixture = CountFixture.withFinalElection
    val preferenceTree = fixture.preferenceTree
    val candidates = fixture.candidates

    val outputStream = new ByteArrayOutputStream()

    PreferenceTreeSerialisation.serialise(preferenceTree, outputStream)

    val inputStream = new ByteArrayInputStream(outputStream.toByteArray)

    val deserialisedPreferenceTree = PreferenceTreeSerialisation.deserialise(candidates, inputStream)

    assert(preferenceTree === deserialisedPreferenceTree)
  }

}
