package chess

import scala.annotation.tailrec
import scala.math.abs
import chess._


sealed case class Pos private (x: Int, y: Int) {

	import Pos.posAt

	lazy val down: Option[Pos] = posAt(x, y - 1)
	lazy val left: Option[Pos] = posAt(x - 1, y)
	lazy val downLeft: Option[Pos] = posAt(x - 1, y - 1)
	lazy val downRight: Option[Pos] = posAt(x + 1, y - 1)
	lazy val up: Option[Pos] = posAt(x, y + 1)
	lazy val right: Option[Pos] = posAt(x + 1, y)
	lazy val upLeft: Option[Pos] = posAt(x - 1, y + 1)
	lazy val upRight: Option[Pos] = posAt(x + 1, y + 1)

	def onSameDiagonal(other: Pos): Boolean = abs(x - other.x) == abs(y - other.y)
	def onSameLine(other: Pos): Boolean = (y == other.y) || (x == other.x)

	def findDir(to: Pos): Option[Direction] = {
		if (!onSameLine(to) && !onSameDiagonal(to)) None
		else {
			val dx = if (to.x == x) 0 else if (to.x > x) 1 else -1
			val dy = if (to.y == y) 0 else if (to.y > y) 1 else -1

			(dx, dy) match {
				case (1, 1) =>   Some(_.upRight)
				case (1, -1) =>  Some(_.downRight)
				case (-1, 1) =>  Some(_.upLeft)
				case (-1, -1) => Some(_.downLeft)
				case (1, 0) => Some(_.right)
				case (-1, 0) => Some(_.left)
				case (0, 1) => Some(_.up)
				case (0, -1) => Some(_.down)
				case _ => None
			}
		}
	}

	val file = (96 + x).toChar.toString
	val rank = y.toString
	val key = file + rank

	override val hashCode = 8 * (y - 1) + (x - 1)

}


object Pos {

	val posCache = new Array[Some[Pos]](64)

	def posAt(x: Int, y: Int): Option[Pos] = {
		if (x < 1 || x > 8 || y < 1 || y > 8) None
		else posCache(x + 8 * y - 9)
	}

	def posAt(key: String): Option[Pos] = {
		allByKeys.get(key)
	}

	def posAt(index: Int): Option[Pos] = {
		if (index < 0 || index >= posCache.size) None
		else posCache(index)
	}

	def distance2(from: Pos, to: Pos): Int = {
		(from.x - to.x) * (from.x - to.x) + (from.y - to.y) * (from.y - to.y)
	}

	def checkAll(from: Pos, to: Pos, stop: Pos => Boolean): Boolean = {
		@tailrec
		def rec(pos: Pos, dir: Direction): Boolean = dir(pos) match {
			case Some(next) if next == to => true
			case Some(next) if !stop(next) => rec(next, dir)
			case _ => false
		}

		from.findDir(to).exists(dir => rec(from, dir))
	}

	private[this] def createPos(x: Int, y: Int): Pos = {
		val pos = new Pos(x, y)
		posCache(x + 8 * y - 9) = Some(pos)
		pos
	}

	val A1 = createPos(1, 1)
	val B1 = createPos(2, 1)
	val C1 = createPos(3, 1)
	val D1 = createPos(4, 1)
	val E1 = createPos(5, 1)
	val F1 = createPos(6, 1)
	val G1 = createPos(7, 1)
	val H1 = createPos(8, 1)
	val A2 = createPos(1, 2)
	val B2 = createPos(2, 2)
	val C2 = createPos(3, 2)
	val D2 = createPos(4, 2)
	val E2 = createPos(5, 2)
	val F2 = createPos(6, 2)
	val G2 = createPos(7, 2)
	val H2 = createPos(8, 2)
	val A3 = createPos(1, 3)
	val B3 = createPos(2, 3)
	val C3 = createPos(3, 3)
	val D3 = createPos(4, 3)
	val E3 = createPos(5, 3)
	val F3 = createPos(6, 3)
	val G3 = createPos(7, 3)
	val H3 = createPos(8, 3)
	val A4 = createPos(1, 4)
	val B4 = createPos(2, 4)
	val C4 = createPos(3, 4)
	val D4 = createPos(4, 4)
	val E4 = createPos(5, 4)
	val F4 = createPos(6, 4)
	val G4 = createPos(7, 4)
	val H4 = createPos(8, 4)
	val A5 = createPos(1, 5)
	val B5 = createPos(2, 5)
	val C5 = createPos(3, 5)
	val D5 = createPos(4, 5)
	val E5 = createPos(5, 5)
	val F5 = createPos(6, 5)
	val G5 = createPos(7, 5)
	val H5 = createPos(8, 5)
	val A6 = createPos(1, 6)
	val B6 = createPos(2, 6)
	val C6 = createPos(3, 6)
	val D6 = createPos(4, 6)
	val E6 = createPos(5, 6)
	val F6 = createPos(6, 6)
	val G6 = createPos(7, 6)
	val H6 = createPos(8, 6)
	val A7 = createPos(1, 7)
	val B7 = createPos(2, 7)
	val C7 = createPos(3, 7)
	val D7 = createPos(4, 7)
	val E7 = createPos(5, 7)
	val F7 = createPos(6, 7)
	val G7 = createPos(7, 7)
	val H7 = createPos(8, 7)
	val A8 = createPos(1, 8)
	val B8 = createPos(2, 8)
	val C8 = createPos(3, 8)
	val D8 = createPos(4, 8)
	val E8 = createPos(5, 8)
	val F8 = createPos(6, 8)
	val G8 = createPos(7, 8)
	val H8 = createPos(8, 8)

	val all: List[Pos] = posCache.toList.flatten

	val allByKeys: Map[String, Pos] = all.map(pos => pos.key -> pos).toMap

}
