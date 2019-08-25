package au.id.tmm.countstv.model

import au.id.tmm.utilities.codec.binarycodecs._

import scala.collection.immutable.ArraySeq

package object preferences {
  private[model] val END_OF_STREAM: Int = -1
  private[model] val magicWord: ArraySeq[Byte] = hex"ADE1A1DE"
  private[model] val serialisationVerson: Int = 1
  private[model] val messageDigestAlgorithm = "SHA-512"
}
