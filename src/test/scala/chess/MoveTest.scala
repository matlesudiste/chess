package chess

import org.scalatest.FunSuite
import Pos._


class MoveTest extends FunSuite {
	
	test("data referential") {
		assert(White.white)
		assert(Black.black)

		Pos.all.foreach { pos =>
			assert(pos != null)
			assert(pos.down != null)
			assert(pos.up != null)
			assert(pos.left != null)
			assert(pos.right != null)
			assert(pos.downLeft != null)
			assert(pos.downRight != null)
			assert(pos.upLeft != null)
			assert(pos.upRight != null)
		}

		assert(Piece(White, Queen).hashCode != Piece(Black, Pawn).hashCode)
	}

	def fromEmpty(piece: Piece, origin: Pos): Board = {
		Board.empty.place(piece, origin).get
	}

	def fromEmpty(list: List[(Piece, Pos)], board: Board = Board.empty): Board = list match {
		case head :: tail => fromEmpty(tail, board.place(head._1, head._2).get)
		case Nil => board
	}

	def playGame(list: List[(Pos, Pos)], game: State = State.init): State = {
		def rec(game: State, elt: (Pos,Pos)): State = {
			val list = new RulesEngine(game).legalMoves.filter(move => move.origin == elt._1 && move.dest == elt._2)
			list.head.after
		}

		list.foldLeft(game)(rec)
	}

	def count(board: Board, color: Color): Int = {
		Pos.all.filter(pos => board.color(pos) == Some(color)).size
	}

	def checkMove(board: Board, origin: Pos, refs: Set[Pos]) {
		val piece = board(origin).get
		val list = new RulesEngine(State(board, History(), piece.color)).legalMoves.filter(_.origin == origin)

		assert(list.map(_.piece).toSet === Set(piece))
		assert(list.map(_.dest).toSet === refs)
	}

	def checkTake(before: Board, origin: Pos, dest: Pos) {
		val piece = before(origin).get
		val moves = new RulesEngine(State(before, History(), piece.color)).legalMoves
		val list = moves.filter(move => move.origin == origin && move.dest == dest)

		assert(list.size === 1)

		val after = list.head.after.board

		assert(after(origin).isEmpty)
		assert(!after(dest).isEmpty)
		assert(after(dest).get === piece)

		assert(count(before, piece.color) === count(after, piece.color))
		assert(count(before, !piece.color) === count(after, !piece.color) + 1)
	}

	test("knight move") {
		val piece = Piece(White, Knight)

		val board1 = fromEmpty(piece, C3)
		checkMove(board1, C3, Set(D1, E2, E4, D5, B5, A4, A2, B1))

		val board2 = Board.init
		checkMove(board2, B1, Set(A3, C3))

		val board3 = board2.move(A2, A3).get
		checkMove(board3, B1, Set(C3))
	}

	test("knight capture") {
		val board = fromEmpty(List((Piece(White, Knight), A3), (Piece(Black, Pawn), B5)))
		checkTake(board, A3, B5)
	}

	test("king move") {
		val board1 = fromEmpty(Piece(White, King), C3)
		checkMove(board1, C3, Set(C2, D2, D3, D4, C4, B4, B3, B2))

		val board2 = Board.init.move(E2, E4).get
		checkMove(board2, E1, Set(E2))

		val board3 = board2.move(D2, D3).get
		checkMove(board3, E1, Set(D2, E2))
	}

	test("king capture") {
		val board = fromEmpty(List((Piece(White, King), E1), (Piece(Black, Pawn), E2)))
		checkTake(board, E1, E2)
	}

	test("bishop move") {
		val board1 = fromEmpty(Piece(Black, Bishop), D4)
		checkMove(board1, D4, Set(C5, B6, A7, E3, F2, G1, C3, B2, A1, E5, F6, G7, H8))

		val board2 = Board.init.move(G7, G6).get
		checkMove(board2, F8, Set(G7, H6))

		val board3 = board2.move(E7, E6).get
		checkMove(board3, F8, Set(G7, H6, E7, D6, C5, B4, A3))
	}

	test("bishop capture") {
		val board = fromEmpty(List((Piece(Black, Bishop), F8), (Piece(White, Pawn), A3)))
		checkTake(board, F8, A3)
	}

	test("rook move") {
		val board1 = fromEmpty(Piece(Black, Rook), D4)
		checkMove(board1, D4, Set(A4, B4, C4, E4, F4, G4, H4, D1, D2, D3, D5, D6, D7, D8))

		val board2 = Board.init.move(A7, A5).get
		checkMove(board2, A8, Set(A7, A6))

		val board3 = board2.move(A8, A6).get
		checkMove(board3, A6, Set(A7, A8, B6, C6, D6, E6, F6, G6, H6))
	}

	test("rook capture") {
		val board = fromEmpty(List((Piece(Black, Rook), A8), (Piece(White, Pawn), A5)))
		checkTake(board, A8, A5)
	}

	test("queen move") {
		val board1 = fromEmpty(Piece(White, Queen), D4)
		checkMove(board1, D4, Set(A4, B4, C4, E4, F4, G4, H4, D1, D2, D3, D5, D6, D7, D8,
								C5, B6, A7, E3, F2, G1, C3, B2, A1, E5, F6, G7, H8))

		val board2 = fromEmpty(Piece(White, Queen), H8)
		checkMove(board2, H8, Set(H7, H6, H5, H4, H3, H2, H1, A8, B8, C8, D8, E8, F8, G8,
								A1, B2, C3, D4, E5, F6, G7))

		val board3 = Board.init.move(C2, C4).get
		checkMove(board3, D1, Set(C2, B3, A4))

		val board4 = board3.move(D2, D4).get
		checkMove(board4, D1, Set(C2, B3, A4, D2, D3))
	}

	test("queen capture") {
		val board = fromEmpty(List((Piece(White, Queen), B3), (Piece(Black, Pawn), F7)))
		checkTake(board, B3, F7)
	}

	test("pawn move") {
		val board1 = fromEmpty(Piece(White, Pawn), D2)
		checkMove(board1, D2, Set(D3, D4))

		val board2 = fromEmpty(Piece(White, Pawn), D3)
		checkMove(board2, D3, Set(D4))
	}

	test("pawn capture") {
		val board = fromEmpty(List((Piece(Black, Pawn), D5), (Piece(White, Pawn), E4)))
		checkTake(board, D5, E4)
	}

	test("pawn promotion into queen") {
		val piece = Piece(White, Pawn)

		val board = fromEmpty(piece, D7)

		val list = new RulesEngine(State(board, History(), piece.color)).legalMoves

		assert(list.size === 1)

		val move = list.head

		assert(move.piece === piece)
		assert(move.origin === D7)
		assert(move.dest === D8)
		assert(move.after.board(D8) === Some(Piece(White, Queen)))
	}

	test("pawn promotion into all kinds of roles") {
		val piece = Piece(White, Pawn)

		val board = fromEmpty(piece, D7)

		val list = new RulesEngine(State(board, History(), piece.color)).legalMoves

		assert(list.size === 4)

		val move = list.head

		assert(list.map(_.piece).toSet === Set(piece))
		assert(list.map(_.origin).toSet === Set(D7))
		assert(list.map(_.dest).toSet === Set(D8))
		assert(list.map(move => move.after.board(move.dest)).toSet === Set(Some(Piece(White, Queen)),
																		Some(Piece(White, Rook)),
																		Some(Piece(White, Bishop)),
																		Some(Piece(White, Knight))))
	}

	test("prise en passant - black side") {
		val initialGame = playGame(List((G2, G3), (D7, D5),
								(A2, A3), (D5, D4),
								(A3, A4), (H7, H5),
								(A4, A5), (H5, H4)))

		val game = playGame(List((E2, E4)), initialGame)
		val list = new RulesEngine(game).legalMoves.filter(move => move.origin == D4 && move.dest == E3)

		assert(list.size === 1)
		assert(list.head.piece === Piece(Black, Pawn))

		val after = list.head.after.board

		assert(!after.contains(E4))
		assert(count(game.board, Black) === count(after, Black))
		assert(count(game.board, White) === count(after, White) + 1)

		val list2 = new RulesEngine(playGame(List((G3, G4)), initialGame))
						.legalMoves.filter(move => move.origin == D5 && move.dest == E6)

		assert(list2.isEmpty)
	}

	test("prise en passant - white side") {
		val initialGame = playGame(List((D2, D4), (G7, G6),
								(D4, D5), (A7, A6),
								(H2, H4), (A6, A5),
								(H4, H5)))

		val game = playGame(List((E7, E5)), initialGame)
		val list = new RulesEngine(game).legalMoves.filter(move => move.origin == D5 && move.dest == E6)

		assert(list.size === 1)
		assert(list.head.piece === Piece(White, Pawn))

		val after = list.head.after.board

		assert(!after.contains(E5))
		assert(count(game.board, Black) === count(after, Black) + 1)
		assert(count(game.board, White) === count(after, White))

		val list2 = new RulesEngine(playGame(List((G6, G5)), initialGame))
						.legalMoves.filter(move => move.origin == D5 && move.dest == E6)

		assert(list2.isEmpty)
	}

	def checkCastle(color: Color, game: State, kingFrom: Pos, kingTo: Pos, rookFrom: Pos, rookTo: Pos) = {
		val list = new RulesEngine(game).legalMoves.filter(move => move.origin == kingFrom && move.dest == kingTo)

		assert(list.size === 1)

		val move = list.head

		assert(move.piece === Piece(color, King))
		assert(!move.after.board.contains(kingFrom))

		val dir = rookFrom.findDir(rookTo).get
		val rookMove = List(rookFrom, dir(rookFrom).get).filter(p => p != rookTo && p != kingTo)
		for (pos <- rookMove) assert(!move.after.board.contains(pos))

		assert(move.after.board(rookTo) === Some(Piece(color, Rook)))
		assert(move.after.board(kingTo) === Some(Piece(color, King)))

		val history = move.after.history

		assert(history.allowed(color, true) === false)
		assert(history.allowed(color, false) === false)
	}

	test("castle") {
		val shortCastleWhite = playGame(List((G2, G3), (G7, G6), (G1, F3), (G8, F6),
											(F1, H3), (F8, H6)))

		val shortCastleBlack = playGame(List((A2, A3)), shortCastleWhite)

		val longCastleWhite = playGame(List((B2, B3), (B7, B6), (B1, C3), (B8, C6),
											(C1, A3), (C8, A6), (E2, E3), (E7, E6),
											(D1, F3), (D8, F6)))

		val longCastleBlack = playGame(List((H2, H3)), longCastleWhite)

		checkCastle(White, shortCastleWhite, E1, G1, H1, F1)
		checkCastle(Black, shortCastleBlack, E8, G8, H8, F8)
		checkCastle(White, longCastleWhite, E1, C1, A1, D1)
		checkCastle(Black, longCastleBlack, E8, C8, A8, D8)
	}

	test("short castle not allowed after rook or king moved") {
		val shortCastle = playGame(List((G2, G3), (G7, G6), (G1, F3), (G8, F6),
										(E2, E3), (E7, E6), (F1, D3), (F8, D6),
										(H2, H3), (H7, H6)))

		assert(!new RulesEngine(shortCastle).legalMoves.filter(move => move.origin == E1 && move.dest == G1).isEmpty)

		val whiteKingMoved = playGame(List((E1, E2), (E8, E7), (E2, E1), (E7, E8)), shortCastle)
		assert(new RulesEngine(whiteKingMoved).legalMoves.filter(move => move.origin == E1 && move.dest == G1).isEmpty)

		val shortCastleBlack = playGame(List((A2, A3)), shortCastle)
		assert(!new RulesEngine(shortCastleBlack).legalMoves.filter(move => move.origin == E8 && move.dest == G8).isEmpty)

		val blackRookMoved = playGame(List((H1, H2), (H8, H7), (H2, H1), (H7, H8), (A2, A3)), shortCastle)
		assert(new RulesEngine(blackRookMoved).legalMoves.filter(move => move.origin == E8 && move.dest == G8).isEmpty)
	}

	test("long castle not allowed after rook or king moved") {
		val longCastle = playGame(List((B2, B3), (B7, B6), (B1, C3), (B8, C6),
										(D2, D3), (D7, D6), (C1, F4), (C8, F5),
										(E2, E3), (E7, E6), (D1, F3), (D8, F6),
										(A2, A3), (A7, A6)))

		val longCastleBlack = playGame(List((H2, H3)), longCastle)
		assert(!new RulesEngine(longCastleBlack).legalMoves.filter(move => move.origin == E8 && move.dest == C8).isEmpty)

		val blackKingMoved = playGame(List((E1, E2), (E8, E7), (E2, E1), (E7, E8), (H2, H3)), longCastle)
		assert(new RulesEngine(blackKingMoved).legalMoves.filter(move => move.origin == E8 && move.dest == G8).isEmpty)

		assert(!new RulesEngine(longCastle).legalMoves.filter(move => move.origin == E1 && move.dest == C1).isEmpty)

		val whiteRookMoved = playGame(List((A1, A2), (A8, A7), (A2, A1), (A7, A8)), longCastle)
		assert(new RulesEngine(whiteRookMoved).legalMoves.filter(move => move.origin == E1 && move.dest == C1).isEmpty)
	}

	test("castle forbidden when king is check") {
		val board = fromEmpty(List((Piece(White, King), E1),
									(Piece(White, Rook), A1),
									(Piece(White, Rook), H1),
									(Piece(Black, Queen), E8)))

		checkMove(board, E1, Set(D1, D2, F1, F2))
	}

	test("get king out of check") {
		val board = fromEmpty(List((Piece(White, King), E1),
									(Piece(Black, Rook), A1),
									(Piece(Black, Bishop), C3)))

		checkMove(board, E1, Set(E2, F2))
	}

	test("do not put king in check") {
		val board1 = fromEmpty(List((Piece(Black, King), E8),
									(Piece(White, Queen), H7),
									(Piece(White, Knight), G6)))

		checkMove(board1, E8, Set(D8))

		val board2 = fromEmpty(List((Piece(Black, King), E8),
									(Piece(White, King), E6),
									(Piece(White, Pawn), G7)))

		checkMove(board1, E8, Set(D8))
	}

	test("put piece in between to stop check") {
		val board = fromEmpty(List((Piece(Black, King), E8),
									(Piece(Black, Knight), F7),
									(Piece(White, Queen), A8),
									(Piece(White, King), E6)))

		val list = new RulesEngine(State(board, History(), Black)).legalMoves

		assert(list.size === 1)
		assert(list.head.piece === Piece(Black, Knight))
		assert(list.head.origin === F7)
		assert(list.head.dest === D8)
	}

	test("game status draw") {
		val board = fromEmpty(List((Piece(Black, King), E8),
									(Piece(White, Rook), A7),
									(Piece(White, Bishop), H4),
									(Piece(White, Knight), H7)))

		val game = new RulesEngine(State(board, History(), Black))

		assert(game.kingInCheck === false)
		assert(game.legalMoves.isEmpty)
		assert(game.gameStatus === Some(Draw))
	}

	test("game status mate") {
		val board = fromEmpty(List((Piece(White, King), E1),
									(Piece(Black, Rook), A1),
									(Piece(Black, Rook), A2)))

		val game = new RulesEngine(State(board, History(), White))

		assert(game.kingInCheck === true)
		assert(game.legalMoves.isEmpty)
		assert(game.gameStatus === Some(Mate))
	}

}
