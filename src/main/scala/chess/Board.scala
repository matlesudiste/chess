package chess

import Pos.posAt


case class Board(pieces: Array[Option[Piece]], hash: Long) {

	def apply(at: Pos): Option[Piece] = pieces(at.hashCode)

	def apply(x: Int, y: Int): Option[Piece] = posAt(x, y).flatMap(at => apply(at))

	def color(at: Pos): Option[Color] = pieces(at.hashCode).map(_.color)

	def contains(at: Pos): Boolean = !pieces(at.hashCode).isEmpty

	def exists(predicate: (Pos, Piece) => Boolean): Boolean = {
		Board.index.exists(i => !pieces(i).isEmpty && predicate(Pos.posAt(i).get, pieces(i).get))
	}

	def find(predicate: (Pos, Piece) => Boolean): Option[Pos] = {
		Board.index.find(i => !pieces(i).isEmpty && predicate(Pos.posAt(i).get, pieces(i).get))
			.flatMap(i => Pos.posAt(i))
	}

	def flatMap[K](fct: (Pos, Piece) => List[K]): List[K] = {
		Board.index.flatMap(i => {
			if (pieces(i).isEmpty) Nil
			else fct(Pos.posAt(i).get, pieces(i).get)
		})
	}

	def place(piece: Piece, at: Pos): Option[Board] = {
		if (contains(at)) None
		else Some(Board(pieces.updated(at.hashCode, Some(piece)),
						updateHash(hash, at, Some(piece))))
	}

	def replace(piece: Piece, at: Pos): Option[Board] = {
		if (!contains(at)) None
		else Some(Board(pieces.updated(at.hashCode, Some(piece)),
						updateHash(hash, at, Some(piece))))
	}

	def remove(at: Pos): Option[Board] = {
		if (!contains(at)) None
		else Some(Board(pieces.updated(at.hashCode, None),
						updateHash(hash, at, None)))
	}

	def move(orig: Pos, dest: Pos): Option[Board] = {
		if (!contains(orig) || contains(dest)) None
		else {
			val cloneArray = pieces.clone
			cloneArray(dest.hashCode) = cloneArray(orig.hashCode)
			cloneArray(orig.hashCode) = None

			Some(Board(cloneArray,
					{ 
						val hash1 = updateHash(hash, orig, None)
						updateHash(hash1, dest, cloneArray(dest.hashCode))
					} ))
		}
	}

	def take(orig: Pos, dest: Pos): Option[Board] = {
		if (!contains(orig) || !contains(dest)) None
		else {
			val cloneArray = pieces.clone
			cloneArray(dest.hashCode) = cloneArray(orig.hashCode)
			cloneArray(orig.hashCode) = None

			Some(Board(cloneArray,
					{ 
						val hash1 = updateHash(hash, orig, None)
						updateHash(hash1, dest, cloneArray(dest.hashCode))
					} ))
		}
	}

	def take(orig: Pos, dest: Pos, taken: Pos): Option[Board] = {
		if (!contains(orig) || contains(dest) || !contains(taken)) None
		else {
			val cloneArray = pieces.clone
			cloneArray(dest.hashCode) = cloneArray(orig.hashCode)
			cloneArray(orig.hashCode) = None
			cloneArray(taken.hashCode) = None

			Some(Board(cloneArray,
					{ 
						val hash1 = updateHash(hash, orig, None)
						val hash2 = updateHash(hash1, taken, None)
						updateHash(hash2, dest, cloneArray(dest.hashCode))
					} ))
		}
	}

	def updateHash(acc: Long, pos: Pos, piece: Option[Piece]): Long = {
		ZobristHashChess.replace(acc, pos, apply(pos), piece)
	}

	override val hashCode: Int = hash.hashCode

	override def equals(obj: Any) = obj match {
		case board: Board => pieces.sameElements(board.pieces) && (hash == board.hash)
		case _ => false
	}

}


object Board {

	def init: Board = {
		val backRank = List(Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook)
		val array = Array.fill[Option[Piece]](64)(None)

		for (y <- Seq(1, 2, 7, 8); x <- 1 to 8)
			yield (posAt(x, y) map { pos => y match {
				case 1 => array(pos.hashCode) = Some(Piece(White, backRank(x-1)))
				case 2 => array(pos.hashCode) = Some(Piece(White, Pawn))
				case 7 => array(pos.hashCode) = Some(Piece(Black, Pawn))
				case 8 => array(pos.hashCode) = Some(Piece(Black, backRank(x-1)))
			}})

		Board(array, ZobristHashChess.computeHash(array))
	}

	def empty: Board = {
		val array = Array.fill[Option[Piece]](64)(None)
		Board(array, ZobristHashChess.computeHash(array))
	}

	val index = (0 until 64).toList

}
