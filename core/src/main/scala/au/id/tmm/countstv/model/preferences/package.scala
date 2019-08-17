package au.id.tmm.countstv.model

import au.id.tmm.utilities.encoding.EncodingUtils.StringConversions

package object preferences {
  private[model] val END_OF_STREAM: Int = -1
  private[model] val magicWord: Vector[Byte] = "ADE1A1DE".fromHex.toVector
  private[model] val serialisationVerson: Int = 1
  private[model] val messageDigestAlgorithm = "SHA-512"
}
