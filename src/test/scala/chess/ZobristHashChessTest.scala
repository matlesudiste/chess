package chess

import org.scalatest.FunSuite
import Pos._


class ZobristHashChessTest extends FunSuite {
	
	test("hash with Board.place") {
		val firstBoard = Board.init
		val nextBoard = firstBoard.place(Piece(White, Pawn), A4).get

		assert(nextBoard.hash === ZobristHashChess.computeHash(nextBoard.pieces))
	}

	test("hash with Board.replace") {
		val firstBoard = Board.init
		val nextBoard = firstBoard.replace(Piece(White, Queen), H1).get

		assert(nextBoard.hash === ZobristHashChess.computeHash(nextBoard.pieces))
	}

	test("hash with Board.remove") {
		val firstBoard = Board.init
		val nextBoard = firstBoard.remove(C8).get

		assert(nextBoard.hash === ZobristHashChess.computeHash(nextBoard.pieces))
	}

	test("hash with Board.move") {
		val firstBoard = Board.init
		val nextBoard = firstBoard.move(H7, H5).get

		assert(nextBoard.hash === ZobristHashChess.computeHash(nextBoard.pieces))
	}

	test("hash with Board.take") {
		val firstBoard = Board.init
		val firstBoard1 = firstBoard.place(Piece(White, Pawn), D6).get

		val nextBoard = firstBoard1.take(C7, E5, D6).get

		assert(nextBoard.hash === ZobristHashChess.computeHash(nextBoard.pieces))
	}

	test("hash with Board.take v2") {
		val firstBoard = Board.empty
		val firstBoard1 = firstBoard.place(Piece(White, Bishop), D6).get
		val firstBoard2 = firstBoard1.place(Piece(Black, Bishop), G3).get

		val nextBoard = firstBoard2.take(D6, G3).get

		assert(nextBoard.hash === ZobristHashChess.computeHash(nextBoard.pieces))
	}

	test("en passant hash code") {
		val state1 = State.init
		val state2 = State(state1.board, state1.history.copy(enPassant = Some(Pos.A4)), state1.sideToPlay)

		assert(state1.hash != state2.hash)
	}

}
