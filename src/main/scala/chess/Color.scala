package chess


sealed trait Color {

	val white = this == White
	val black = this == Black

	val unary_! : Color

}

case object White extends Color {

	val unary_! = Black
	override val hashCode = 1

}

case object Black extends Color {

	val unary_! = White
	override val hashCode = 2

}
