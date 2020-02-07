package chess

import java.util.concurrent.ThreadLocalRandom
import org.scalatest.FunSuite
import scala.util.parsing.combinator._
import scala.io.Source
import scala.math.abs
import chess._
import Pos._


class EndingsParserTest extends FunSuite {
	
	test("parse board config") {
		val txt = "########## KBBBBvK ##########"
		val result = EndingsParser.parseAll(EndingsParser.boardConfig, txt)

		result match {
			case EndingsParser.Success("KBBBBvK", _) => succeed
			case _ => fail(result.toString)
		}
	}

	test("parse wins") {
		val txt = "2880881736 positions are wins."
		val result = EndingsParser.parseAll(EndingsParser.wins, txt)

		result match {
			case EndingsParser.Success(2880881736L, _) => succeed
			case _ => fail(result.toString)
		}
	}

	test("parse games") {
		val txt = "396411372 positions win in 10 ply."
		val result = EndingsParser.parseAll(EndingsParser.wonGames, txt)

		result match {
			case EndingsParser.Success((396411372L, 10), _) => succeed
			case _ => fail(result.toString)
		}
	}

	test("parse white move") {
		val txt = """White to move:
					69493848 positions win in 1 ply.
					142831284 positions win in 2 ply.
					2880881736 positions are wins.
					449234964 positions are draws.
					0 positions are cursed losses."""

		val result = EndingsParser.parseAll(EndingsParser.whiteMove, txt)

		result match {
			case EndingsParser.Success((2880881736L, 449234964L, 0L), _) => succeed
			case _ => fail(result.toString)
		}
	}

	test("parse longest win") {
		val txts = List("Longest win for black: 3 ply; 8/8/8/8/8/1k6/8/BKB2B1b b - -",
						"Longest cursed win for black: 3 ply; 8/8/8/8/8/1k6/8/BKB2B1b b - -")

		for (txt <- txts) {
			val result = EndingsParser.parseAll(EndingsParser.longestWin, txt)

			result match {
				case EndingsParser.Success("ok", _) => succeed
				case _ => fail(result.toString)
			}
		}
	}

	test("parse position") {
		def check(d1: Double, d2: Double): Boolean = abs(d1 - d2) <= 0.001

		def compareStats(stats1: (Double, Double, Double), stats2: (Double, Double, Double)): Boolean = {
			check(stats1._1, stats2._1) &&
			check(stats1._2, stats2._2) &&
			check(stats1._3, stats2._3)
		}

		val txt = """########## KBBBvKB ##########
					White to move:
					1226138256 positions win in 1 ply.
					379062 positions win in 2 ply.
					78368343 positions win in 3 ply.
					1918947891 positions are wins.
					1999274535 positions are draws.
					118842 positions are losses.
					82236 positions lose in 0 ply.
					36606 positions lose in 2 ply.
					Black to move:
					334566 positions win in 1 ply.
					114576 positions win in 3 ply.
					449142 positions are wins.
					3961763883 positions are draws.
					1309250379 positions are losses.
					5439072 positions lose in 0 ply.
					725370 positions lose in 1 ply.
					360403578 positions lose in 2 ply.
					Longest win for white: 40 ply; 8/8/5b2/8/3k4/8/1K1B3B/7B b - -
					Longest win for black: 3 ply; 8/8/8/8/8/1k6/8/BKB2B1b b - -"""

		val result = EndingsParser.parseAll(EndingsParser.position, txt)

		val whiteSum = (1918947891L + 1999274535L + 118842L).toDouble
		val whiteRef = (1918947891L / whiteSum, (1918947891L + 1999274535L) / whiteSum, 1.0)

		val blackSum = (449142L + 3961763883L + 1309250379L).toDouble
		val blackRef = (449142L / blackSum, (449142L + 3961763883L) / blackSum, 1.0)

		val ref = Map(("KBBBvKB", White) -> whiteRef, ("KBBBvKB", Black) -> blackRef)

		result match {
			case EndingsParser.Success(stats, _) => {
				assert(stats.keySet === ref.keySet)
				for (key <- ref.keySet) assert(compareStats(stats(key), ref(key)))
			}
			case _ => {
				fail(result.toString)
			}
		}
	}

	test("parse complete file") {
		val txt = Source.fromResource("3-4-5.txt").mkString
		val stats = EndingsParser.parse(txt)

		assert(!stats.isEmpty)
		assert(stats.get.size === 1018)

		for (stat <- stats.get.values) {
			assert(stat._1 >= 0)
			assert(stat._2 >= stat._1)
			assert(stat._3 >= stat._2)
			assert(stat._3 <= 1.0)
		}
	}

	def fromEmpty(list: List[(Piece, Pos)], board: Board = Board.empty): Board = list match {
		case head :: tail => fromEmpty(tail, board.place(head._1, head._2).get)
		case Nil => board
	}

	def addRandomPiece(board: Board, n: Int): Board = {
		if (n <= 0) board
		else {
			val remainingPos = Pos.all.filter(pos => !board.contains(pos))
			val pos = remainingPos(ThreadLocalRandom.current.nextInt(remainingPos.size))

			val color = if (ThreadLocalRandom.current.nextDouble > 0.5 ) White else Black

			val roles = Role.all.filter(_ != King)
			val role = roles(ThreadLocalRandom.current.nextInt(roles.size))

			addRandomPiece(board.place(Piece(color, role), pos).get, n-1)
		}
	}

	test("boardToCode") {
		val txt = Source.fromResource("3-4-5.txt").mkString
		val stats = EndingsParser.parse(txt)

		val endings = StatsEndings(stats.get)
		val code = endings.boardToCode(Board.init)

		assert(code === "KQBBNNRRPPPPPPPPvKQBBNNRRPPPPPPPP")
	}

	test("invert code and stat") {
		val endings = StatsEndings(Map.empty)

		assert(endings.invertCode("KPvK") === "KvKP")
		assert(endings.invertStat((0.1, 0.6, 1.0)) === (0.4, 0.9, 1.0))
	}

	test("stats for all possible boards of 5 pieces") {
		val txt = Source.fromResource("3-4-5.txt").mkString
		val stats = EndingsParser.parse(txt)
		val endings = StatsEndings(stats.get)

		val twoKings = fromEmpty(List((Piece(White, King), E1), (Piece(Black, King), E8)))

		val missingCodes = scala.collection.mutable.ListBuffer.empty[String]

		for (i <- 0 to 10000) {
			val board = addRandomPiece(twoKings, 3)
			val code = endings.boardToCode(board)

			if (!endings.completeStats.contains((code, White)) || !endings.completeStats.contains((code, Black))) {
				if (!missingCodes.contains(code) && !missingCodes.contains(endings.invertCode(code))) {
					missingCodes += code
				}
			}
		}

		if (missingCodes.isEmpty) succeed
		else {
			for (code <- missingCodes) println("missing code " + code)
			fail(missingCodes.size + " missing codes")
		}
	}

}
