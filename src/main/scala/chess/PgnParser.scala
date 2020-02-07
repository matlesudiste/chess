package chess

import scala.util.parsing.combinator._
import chess._


object PgnParser extends RegexParsers {

	val tagName: Parser[String] = "[" ~> """[a-zA-Z]+""".r

	val tagValue: Parser[String] = """[^\]]+""".r <~ "]" ^^ {
		_.replace("\"", "")
	}

	def tag: Parser[(String, String)] = tagName ~ tagValue ^^ {
		case name ~ value => (name -> value)
	}

	def tags: Parser[PgnTags] = rep(tag) ^^ {
		_.toMap
	}

	val blockCommentary: Parser[String] = "{" ~> """[^\}]*""".r <~ "}"

	val inlineCommentary: Parser[String] = ";" ~> """.+""".r

    val commentary: Parser[String] = blockCommentary | inlineCommentary

	val number: Parser[String] = """[1-9]\d*[\s\.]*""".r

	val moveRegex = """(?:(?:0\-0(?:\-0|)[\+\#]?)|[PQKRBNOoa-h@][QKRBNa-h1-8xOo\-=\+\#\@]{1,6})[\?!□]{0,2}""".r

	val nag: Parser[String] = """\$\d+""".r

	val nagGlyphs: Parser[List[String]] = rep(nag)

	val variation: Parser[List[String]] = "(" ~> strMoves <~ ")"

	def strMove: Parser[String] = {
		((number | commentary)*) ~> (moveRegex ~ nagGlyphs ~ rep(commentary) ~ rep(variation)) <~ (commentary*) ^^ {
			case san ~ glyphs ~ comments ~ variations => san
		}
	}

	val result: Parser[String] = "*" | "1/2-1/2" | "½-½" | "0-1" | "1-0"

	def strMoves: Parser[PgnMoves] = {
		(commentary*) ~ (strMove*) ~ (result) ~ (commentary*) ^^ {
			case coms ~ sans ~ res ~ _ => sans
		}
	}

	def singleGame: Parser[(PgnTags, PgnMoves)] = tags ~ strMoves ^^ {
		case tg ~ sans => (tg, sans)
	}

	def multipleGames: Parser[List[(PgnTags, PgnMoves)]] = rep(singleGame) ^^ {
		_.toList
	}

	def parseTags(pgn: String): Option[PgnTags] = parseAll(tags, pgn) match {
		case Success(tags, _) => Some(tags)
		case failure : NoSuccess => None
	}

	def parseMoves(pgn: String): Option[PgnMoves] = parseAll(strMoves, pgn) match {
		case Success(strMoves, _) => Some(strMoves)
		case failure : NoSuccess => None
	}

	def parseSingleGame(pgn: String): Option[(PgnTags, PgnMoves)] = parseAll(singleGame, pgn) match {
		case Success(game, _) => Some(game)
		case failure : NoSuccess => None
	}

	def parseMultipleGames(pgn: String): Option[List[(PgnTags, PgnMoves)]] = parseAll(multipleGames, pgn) match {
		case Success(games, _) => Some(games)
		case failure : NoSuccess => None
	}

}
