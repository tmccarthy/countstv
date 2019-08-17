package au.id.tmm.countstv.model.preferences

import java.io.{InputStream, OutputStream}

import au.id.tmm.countstv.model.preferences.PreferenceTree.RootPreferenceTree

object PreferenceTreeSerialisation {

  def serialise[C](preferenceTree: RootPreferenceTree[C], outputStream: OutputStream): Unit = {
    PreferenceTableSerialisation.serialiseAndCompress(preferenceTree.preferenceTable, outputStream)
  }

  def deserialise[C <: AnyRef : Ordering](allCandidates: Set[C], inputStream: InputStream): RootPreferenceTree[C] = {
    PreferenceTableDeserialisation.decompressAndDeserialise(allCandidates, inputStream).map { preferenceTable =>
      new RootPreferenceTree[C](preferenceTable)
    }.fold(e => throw e, tree => tree)
  }
}
