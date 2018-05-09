package au.id.tmm.countstv

import scala.collection.parallel.immutable.ParSet

package object counting {

  private[counting] type PaperBundles[C] = ParSet[PaperBundle[C]]

}
