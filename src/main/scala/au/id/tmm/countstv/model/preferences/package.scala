package au.id.tmm.countstv.model

package object preferences {
  private[model] val END_OF_STREAM: Int = -1
  private[model] val magicWord: Vector[Int] = Vector(0xADE1, 0xA1DE)
  private[model] val serialisationVerson: Int = 1
  private[model] val messageDigestAlgorithm = "SHA-512"
}
