package chess

import org.scalatest.FunSuite
import chess._
import Pos._


class MasterGameTest extends FunSuite {
	
	test("load master games resource") {
		assert(!MasterGame.loadPgnResource(MasterGame.smallNbOfGames).isEmpty)
	}

	test("simple move") {
		val moves = MasterGame.buildMoves(List("d4"))

		assert(!moves.isEmpty)

		val board = moves.head.after.board

		assert(!board.contains(D2))
		assert(board(D4) === Some(Piece(White, Pawn)))
	}

	test("default piece to move is pawn") {
		val moves = MasterGame.buildMoves(List("c3"))

		assert(!moves.isEmpty)

		val board = moves.head.after.board

		assert(board(C3) === Some(Piece(White, Pawn)))
	}

	test("moves with role") {
		val moves = MasterGame.buildMoves(List("Nf3", "g5"))

		assert(moves.size == 2)

		val board = moves.last.after.board

		assert(!board.contains(G1))
		assert(board(F3) === Some(Piece(White, Knight)))
		assert(!board.contains(G7))
		assert(board(G5) === Some(Piece(Black, Pawn)))
	}

	test("moves with take") {
		val moves = MasterGame.buildMoves(List("Nf3", "g5", "Nxg5"))

		assert(moves.size == 3)

		val board = moves.last.after.board

		assert(!board.contains(G1))
		assert(!board.contains(F3))
		assert(!board.contains(G7))
		assert(board(G5) === Some(Piece(White, Knight)))
	}

	test("move with origin") {
		val list = List("e4", "c5", "Nf3", "Nc6", "Bb5", "g6", "Bxc6", "dxc6")
		val moves = MasterGame.buildMoves(list)

		assert(moves.size === list.size,
				"failed to parse move : " + list(List(moves.size, list.size-1).min))

		assert(moves.last.piece === Piece(Black, Pawn))
		assert(moves.last.origin === D7)
		assert(moves.last.dest === C6)
	}

	test("short castle") {
		val list = List("d4", "d5", "Bf4", "Nc6", "e3", "Bf5", "Nf3", "e6", "a3", "Nf6",
						"c4", "Be7", "Nc3", "O-O")

		val moves = MasterGame.buildMoves(list)

		assert(moves.size === list.size,
				"failed to parse move : " + list(List(moves.size, list.size-1).min))

		assert(moves.last.piece === Piece(Black, King))
		assert(moves.last.origin === E8)
		assert(moves.last.dest === G8)
	}

	test("long castle") {
		val list = List("d4", "Nf6", "Bg5", "e6", "e4", "Be7", "Nd2", "d5", "e5", "Nfd7",
						"Be3", "b6", "Nh3", "Bb7", "Qg4", "g6", "Ng5", "Nf8", "h4", "h6",
						"Ngf3", "Na6", "c3", "Qd7", "a4", "O-O-O")

		val moves = MasterGame.buildMoves(list)

		assert(moves.size === list.size,
				"failed to parse move : " + list(List(moves.size, list.size-1).min))

		assert(moves.last.piece === Piece(Black, King))
		assert(moves.last.origin === E8)
		assert(moves.last.dest === C8)
	}

	test("pawn promotion into queen") {
		val list = List("e4", "c5", "Nf3", "d6", "d4", "cxd4", "Nxd4", "Nf6", "Nc3", "e6",
						"f4", "a6", "Qf3", "Qb6", "Nb3", "Qc7", "Bd3", "b5", "g4", "b4",
						"Ne2", "Bb7", "g5", "Nfd7", "Bd2", "Nc6", "Nbd4", "Nc5", "Nxc6", "Qxc6",
						"Nd4", "Qd7", "O-O-O", "Qa4", "Kb1", "b3", "Nxb3", "Nxe4", "Qf1", "g6",
						"Be1", "Bg7", "h4", "O-O", "h5", "Nc5", "Rh4", "Nxd3", "Rxd3", "Be4",
						"hxg6", "fxg6", "Rc3", "Rac8", "Rh2", "Bf5", "Qf2", "Bxc3", "Bxc3", "e5",
						"fxe5", "Bd3", "Qe3", "Rf1+", "Nc1", "Rxc3", "bxc3", "Qb5+", "Ka1", "Bc4",
						"exd6", "Bf7", "Rf2", "Rxf2", "Qxf2", "Qxg5", "Kb2", "h5", "Qd4", "h4",
						"Nd3", "h3", "d7", "Qd8", "Ne5", "h2", "Nc6", "Qxd7", "Qxd7", "h1=Q")

		val moves = MasterGame.buildMoves(list)

		assert(moves.size === list.size,
				"failed to parse move : " + list(List(moves.size, list.size-1).min))

		assert(moves.last.piece === Piece(Black, Pawn))
		assert(moves.last.origin === H2)
		assert(moves.last.dest === H1)
	}

	test("scholar's mate") {
		val list = List("e4", "e5", "Bc4", "Nc6", "Qh5", "Nf6", "Qxf7#")
		val moves = MasterGame.buildMoves(list)

		assert(moves.size === list.size,
				"failed to parse move : " + list(List(moves.size, list.size-1).min))

		val state = new RulesEngine(moves.last.after)

		assert(state.gameStatus === Some(Mate))
	}

	val completeGame = List("d4", "d5", "Bf4", "Nc6", "e3", "Bf5", "Nf3", "e6", "a3", "Nf6",
							"c4", "Be7", "Nc3", "O-O", "cxd5", "exd5", "Bd3", "Bg4", "Qc2", "g6",
							"h3", "Bf5", "Bxf5", "gxf5", "Qxf5", "Kh8", "Ne5", "Nxe5", "Bxe5", "Rg8",
							"g4", "h6", "h4", "Rg6", "g5", "h5", "Nxd5", "Kg7", "Nxe7", "Qxe7",
							"gxf6+", "Rxf6", "Rg1+", "Kf8", "Bxf6", "Qd6", "Be5", "Qd5", "Bg7+", "Ke8",
							"Qxd5", "Rd8", "Qf5", "b6", "Bf6", "Rd6", "Rg8#")

	test("complete game") {
		val moves = MasterGame.buildMoves(completeGame)

		assert(moves.size === completeGame.size,
			"failed to parse move : " + completeGame(List(moves.size, completeGame.size-1).min))

		val state = new RulesEngine(moves.last.after)

		assert(state.gameStatus === Some(Mate))
	}

	test("build a fake game") {
		val tags = Map("White" -> "Hulk", "Black" -> "Spiderman", "Event" -> "Marvel")
		val result = MasterGame.buildGame(tags, completeGame)

		assert(!result.isEmpty)

		val game = result.get

		assert(game.moves.size === completeGame.size)
		assert(game.gameStatus === Some(Mate))
		assert(game.winner === Some(White))
		assert(game.desc === "Hulk vs Spiderman @ Marvel")
	}

	def checkGames(strGames: List[(PgnTags, PgnMoves)]) {
		val errors = new StringBuilder()

		for (strGame <- strGames) {
			val tags = strGame._1
			val strMoves = strGame._2

			val game = MasterGame.buildGame(tags, strMoves)
			val desc = tags("White") + " vs " + tags("Black")

			if (game.isEmpty) {
				errors.append(desc + " : unable to load game" + "\n")

			} else if (game.get.moves.size != strMoves.size) {
				val move = strMoves(game.get.moves.size)
				errors.append(desc + " : unable to load moves, broken move : " + move + "\n")
			}
		}

		if (!errors.isEmpty) fail(errors.toString)
	}

	test("check all games from small resource") {
		val pgn = MasterGame.loadPgnResource(MasterGame.smallNbOfGames)
		assert(!pgn.isEmpty)

		val strGames = PgnParser.parseMultipleGames(pgn.get)
		assert(!strGames.isEmpty)

		checkGames(strGames.get)
	}

	test("check all games from large resource") {
		val pgn = MasterGame.loadPgnResource(MasterGame.largeNbOfGames)
		assert(!pgn.isEmpty)

		val strGames = PgnParser.parseMultipleGames(pgn.get)
		assert(!strGames.isEmpty)

		checkGames(strGames.get)
	}

}
