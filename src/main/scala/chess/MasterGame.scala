package chess

import scala.io.Source
import scala.util.parsing.combinator._
import Pos._


case class MasterGame(moves: List[Move], gameStatus: Option[Status], winner: Option[Color], desc: String)


class MoveParser(engine: RulesEngine) extends RegexParsers {

	val nextAvailable: Iterable[Move] = engine.nextMoves

	val sideToPlay = engine.state.sideToPlay

	def filter(dest: Pos, role: Option[Role] = None, file: String = "", rank: String = ""): Option[Move] = {
		def filterRole(move: Move): Boolean = role.isEmpty || (move.piece.role == role.get)
		def filterFile(move: Move): Boolean = file.isEmpty || (move.origin.file == file)
		def filterRank(move: Move): Boolean = rank.isEmpty || (move.origin.rank == rank)

		val list = nextAvailable.filter(move => move.dest == dest && filterRole(move)
												&& filterFile(move) && filterRank(move))

		if (list.isEmpty) {
			None
		} else if (list.size == 1) {
			Some(list.head)
		} else if (role.isEmpty) {
			val list2 = list.filter(move => move.piece.role == Pawn)
			if (list2.size == 1) Some(list2.head) else None
		} else {
			None
		}
	}

	val shortCastle: Parser[Option[Move]] = ("O-O+" | "o-o+" | "0-0+" | "O-O" | "o-o" | "0-0") ^^^ {
		val dest = if (sideToPlay == White) G1 else G8
		filter(dest, Some(King))
	}

	val longCastle: Parser[Option[Move]] = ("O-O-O+" | "o-o-o+" | "0-0-0+" | "O-O-O" | "o-o-o" | "0-0-0") ^^^ {
		val dest = if (sideToPlay == White) C1 else C8
		filter(dest, Some(King))
	}

	val role: Parser[Option[Role]] = """(P|N|B|R|Q|K|)""".r ^^ {
		case str => if (str.isEmpty) None else Some(Role.allByPgn(str.charAt(0)))
	}

	val check: Parser[String] = "+"

	val mate: Parser[String] = "#"

	val extentedMove = """([a-h]?)([1-8]?)(x?)([a-h][1-8])(\=Q)?""".r

	val completeMove: Parser[Option[Move]] = role ~ extentedMove ~ (check?) ~ (mate?) ^^ {
		case role ~ extentedMove(file, rank, take, posStr, promotion) ~ check ~ mate => {
			Pos.posAt(posStr).flatMap(pos => filter(pos, role, file, rank))
		}
	}

	def move: Parser[Option[Move]] = longCastle | shortCastle | completeMove

	def apply(str: String): Option[Move] = parseAll(move, str) match {
		case Success(ms, _) => ms
		case failure : NoSuccess => None
	}

}


object MasterGame {

	val smallNbOfGames = "master_games.pgn"
	val largeNbOfGames = "Kasparov.pgn"

	def loadAllGames(resName: String): List[MasterGame] = {
		val pgn = loadPgnResource(resName)

		if (pgn.isEmpty) {
			Nil
		} else {
			val strGames = PgnParser.parseMultipleGames(pgn.get)
			if (strGames.isEmpty) Nil
			else strGames.get.flatMap(strGame => buildGame(strGame._1, strGame._2))
		}
	}

	def loadPgnResource(resourceName: String): Option[String] = {
		if (Thread.currentThread().getContextClassLoader().getResource(resourceName) != null) {
			Some(Source.fromResource(resourceName).mkString)
		} else {
			None
		}
	}

	def buildMoves(nextStr: List[String], previous: List[Move] = Nil): List[Move] = nextStr match {
		case str :: tail => {
			val state = if (previous.isEmpty) State.init else previous.head.after
			val next = new MoveParser(new RulesEngine(state))(str)

			if (next.isEmpty) previous.reverse
			else buildMoves(tail, next.get :: previous)
		}

		case Nil => previous.reverse
	}

	def buildGame(tags: Map[String, String], strGame: List[String]): Option[MasterGame] = {
		val moves = buildMoves(strGame)

		if (moves.isEmpty) {
			None

		} else {
			val lastMove = moves.last
			val gameStatus = new RulesEngine(lastMove.after).gameStatus
			val winner = if (gameStatus == Some(Mate)) Some(lastMove.piece.color) else None
			val desc = tags("White") + " vs " + tags("Black") + " @ " + tags("Event")

			Some(MasterGame(moves, gameStatus, winner, desc))
		}
	}

}
