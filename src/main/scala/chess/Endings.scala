package chess

import java.util.concurrent.ThreadLocalRandom


trait Endings {

	def contains(state: State): Boolean
	def playout(state: State): Option[Color]

}


case class StatsEndings(stats: Map[(String, Color), (Double, Double, Double)]) extends Endings {
	
	// Order: king (K), queen (Q), bishops (B), knights (N), rooks (R), pawn (P)
	val roles: List[Role] = List(King, Queen, Bishop, Knight, Rook, Pawn)
	val whites: List[Piece] = roles.map(role => Piece(White, role))
	val blacks: List[Piece] = roles.map(role => Piece(Black, role))

	val completeStats: Map[(String, Color), (Double, Double, Double)] = {
		val ex = stats.keySet.map(key => (invertCode(key._1), key._2) -> invertStat(stats(key)))
		ex.toMap ++ stats
	}

	def nextDouble: Double = ThreadLocalRandom.current.nextDouble

	def countPieces(board: Board): Int = {
		board.pieces.foldLeft(0) { (acc, elt) => if (!elt.isEmpty) acc + 1 else acc }
	}

	def countPiece(board: Board, piece: Piece): Int = {
		board.pieces.foldLeft(0) { (acc, elt) => if (!elt.isEmpty && elt.get == piece) acc + 1 else acc }
	}

	def invertCode(code: String): String = {
		val index = code.indexOf("v")
		code.substring(index + 1, code.size) + "v" + code.substring(0, index)
	}

	def invertStat(stat: (Double, Double, Double)): (Double, Double, Double) = {
		val win = stat._1
		val draw = stat._2 - stat._1
		val lose = stat._3 - stat._2
		(lose, lose + draw, lose + draw + win)
	}

	def boardToCode(board: Board): String = {
		val w = whites.flatMap(piece => List.fill(countPiece(board, piece))(piece.role.forsythUpper))
		val b = blacks.flatMap(piece => List.fill(countPiece(board, piece))(piece.role.forsythUpper))
		w.mkString + "v" + b.mkString
	}

	def contains(state: State): Boolean = {
		countPieces(state.board) <= 5
	}

	def playout(state: State): Option[Color] = {
		val key = (boardToCode(state.board), state.sideToPlay)

		if (!completeStats.contains(key)) {
			None
		} else {
			val stat = completeStats(key)
			val rand = nextDouble
			if (rand <= stat._1) Some(state.sideToPlay)
			else if (rand <= stat._2) None
			else Some(!state.sideToPlay)
		}
	}

}


object NoEndings extends Endings {

	def contains(state: State): Boolean = false
	def playout(state: State): Option[Color] = None

}
