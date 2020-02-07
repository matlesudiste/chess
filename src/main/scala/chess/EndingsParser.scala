package chess

import scala.util.parsing.combinator._
import chess._


object EndingsParser extends RegexParsers {

	def computeWeight(stats: (Long, Long, Long)): (Double, Double, Double) = {
		val sum = (stats._1 + stats._2 + stats._3).toDouble
		val tmp = (stats._1 / sum, stats._2 / sum, stats._3 / sum)
		(tmp._1, tmp._1 + tmp._2, tmp._1 + tmp._2 + tmp._3)
	}

	val boardConfig: Parser[String] = "##########" ~> """[a-zA-Z]+""".r <~ "##########"

	val wins: Parser[Long] = """[0-9]+""".r <~ ("positions are wins." | "positions are cursed wins." ) ^^ {
		_.toLong
	}

	val draws: Parser[Long] = """[0-9]+""".r <~ "positions are draws." ^^ {
		_.toLong
	}

	val losses: Parser[Long] = """[0-9]+""".r <~ ("positions are losses." | "positions are cursed losses.") ^^ {
		_.toLong
	}

	val wonGames: Parser[(Long, Int)] = """[0-9]+""".r ~ "positions win in" ~ """[0-9]+""".r ~ "ply." ^^ {
		case nb1 ~ label1 ~ nb2 ~ label2 => (nb1.toLong, nb2.toInt)
	}

	val lostGames: Parser[(Long, Int)] = """[0-9]+""".r ~ "positions lose in" ~ """[0-9]+""".r ~ "ply." ^^ {
		case nb1 ~ label1 ~ nb2 ~ label2 => (nb1.toLong, nb2.toInt)
	}

	val whiteMove: Parser[(Long, Long, Long)] = "White to move:" ~ rep(wonGames) ~ (wins*) ~ draws ~ (losses*) ~ rep(lostGames) ^^ {
		case wonGames ~ win ~ draw ~ losses ~ lostGames => (win.sum, draw, losses.sum)
	}

	val blackMove: Parser[(Long, Long, Long)] = "Black to move:" ~ rep(wonGames) ~ (wins*) ~ draws ~ (losses*) ~ rep(lostGames) ^^ {
		case wonGames ~ win ~ draw ~ losses ~ lostGames => (win.sum, draw, losses.sum)
	}

	val longestWin: Parser[String] = "Longest (cursed)*".r ~ """win for (white|black)\: [0-9]+ ply; [0-9a-zA-Z/]+ (b|w) \- \-""".r ^^^ {
		"ok"
	}

	val position: Parser[Map[(String, Color), (Double, Double, Double)]] = boardConfig ~ whiteMove ~ blackMove ~ rep(longestWin) ^^ {
		case config ~ whiteStats ~ blackStats ~ ignored => Map((config, White) -> computeWeight(whiteStats),
																(config, Black) -> computeWeight(blackStats))
	}

	val complete: Parser[Map[(String, Color), (Double, Double, Double)]] = rep(position) ~ 26.toChar.toString ^^ {
		case pos ~ endChar => pos.reduce(_ ++ _)
	}

	def parse(txt: String): Option[Map[(String, Color), (Double, Double, Double)]] = parseAll(complete, txt) match {
		case Success(stats, _) => Some(stats)
		case failure : NoSuccess => None
	}

}
