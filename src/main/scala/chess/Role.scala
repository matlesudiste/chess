package chess

import chess._
import Pos.posAt


sealed trait Role {

	val forsyth: Char
	lazy val forsythUpper: Char = forsyth.toUpper
	lazy val pgn: Char = forsythUpper

	val dirs: Directions

}


case object King extends Role {

	val forsyth = 'k'
	val dirs = Queen.dirs

}


case object Queen extends Role {

	val forsyth = 'q'
	val dirs = Rook.dirs ::: Bishop.dirs

}


case object Rook extends Role {

	val forsyth = 'r'
	val dirs = List(_.up, _.down, _.left, _.right)

}


case object Bishop extends Role {

	val forsyth = 'b'
	val dirs = List(_.upLeft, _.upRight, _.downLeft, _.downRight)

}


case object Knight extends Role {

	val forsyth = 'n'

	val dirs: Directions = List(
		p => posAt(p.x - 1, p.y + 2),
		p => posAt(p.x - 1, p.y - 2),
		p => posAt(p.x + 1, p.y + 2),
		p => posAt(p.x + 1, p.y - 2),
		p => posAt(p.x - 2, p.y + 1),
		p => posAt(p.x - 2, p.y - 1),
		p => posAt(p.x + 2, p.y + 1),
		p => posAt(p.x + 2, p.y - 1)
	)

}


case object Pawn extends Role {

	val forsyth = 'p'
	val dirs = Nil

}


object Role {

	val all: List[Role] = List(King, Queen, Rook, Bishop, Knight, Pawn)

	val allByPgn: Map[Char, Role] = all.map(role => role.pgn -> role).toMap

}
