package chess


sealed trait Status

case object Mate extends Status {
	override val hashCode = 1
}

case object Draw extends Status {
	override val hashCode = 2
}
