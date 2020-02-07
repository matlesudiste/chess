package chess

import chess._


case class Piece(color: Color, role: Role) {

	def is(c: Color) = c == color
	def is(r: Role) = r == role
	def forsyth: Char = if (color == White) role.forsythUpper else role.forsyth

	val pawnMovingDir: Direction = {
		if (role != Pawn) (x => None)
		else if (color == White) (_.up)
		else (_.down)
	}

	val pawnTakingDirs: Directions = {
		if (role != Pawn) Nil
		else if (color == White) List(_.upLeft, _.upRight)
		else List(_.downLeft, _.downRight)
	}

}

object Piece {
	val all: List[Piece] = Role.all.flatMap(role => List(Piece(White, role), Piece(Black, role)))

}
