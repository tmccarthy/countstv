package au.id.tmm.countstv.normalisation

sealed trait Preference

object Preference {
  final case class Numbered(asInt: Int) extends Preference
  case object Tick extends Preference
  case object Cross extends Preference
}
