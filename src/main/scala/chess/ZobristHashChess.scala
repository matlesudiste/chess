package chess

import scala.util.Random


object ZobristHashChess {

	private final val rand = new Random(1)

	final val zobrist: Map[(Pos, Option[Piece]), Long] = {
		val allPieces = None :: Piece.all.map(p => Some(p))

		val seq = for (pos <- Pos.all; piece <- allPieces) yield ((pos, piece) -> rand.nextLong())
		Map(seq: _*)
	}

	final val whiteHash = rand.nextLong
	final val blackHash = rand.nextLong

	final val trueHash = rand.nextLong
	final val falseHash = rand.nextLong

	def colorZobrist(color: Color): Long = {
		if (color == White) whiteHash else blackHash
	}

	def booleanZobrist(predicate: Boolean): Long = {
		if (predicate) trueHash else falseHash
	}

	def computeHash(array: Array[Option[Piece]]): Long = {
		val longs = for (pos <- Pos.all) yield zobrist((pos, array(pos.hashCode)))
		longs.foldLeft(0L) { (acc, elt) => acc ^ elt }
	}

	def replace(prevHash: Long, pos: Pos, oldPiece: Option[Piece], newPiece: Option[Piece]): Long = {
		prevHash ^ zobrist((pos, oldPiece)) ^ zobrist((pos, newPiece))
	}

	def addSide(prevHash: Long, side: Color): Long = {
		prevHash ^ colorZobrist(side)
	}

	def addState(prevHash: Long, history: History, side: Color): Long = {
		(prevHash ^ colorZobrist(side) ^ booleanZobrist(history.whiteShortCastlingAllowed)
										 ^ booleanZobrist(history.whiteLongCastlingAllowed)
										  ^ booleanZobrist(history.blackShortCastlingAllowed)
										   ^ booleanZobrist(history.blackLongCastlingAllowed))
	}

}
