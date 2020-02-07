package chess

import org.scalatest.FunSuite
import chess._
import Pos._


class PosTest extends FunSuite {

	def checkNeighbors(orig: Pos, up: Pos, down: Pos, left: Pos, right: Pos,
						dl: Pos, dr: Pos, ur: Pos, ul: Pos) = {

		assert(orig.up == Some(up))
		assert(orig.down == Some(down))
		assert(orig.left == Some(left))
		assert(orig.right == Some(right))
		assert(orig.downLeft === Some(dl))
		assert(orig.downRight === Some(dr))
		assert(orig.upRight === Some(ur))
		assert(orig.upLeft === Some(ul))
	}

	test("pos down/up left/right neighbors") {
		checkNeighbors(D4, D5, D3, C4, E4, C3, E3, E5, C5)
	}

	test("distance") {
		assert(Pos.distance2(A1, C2) === 5)
	}

	test("findDir") {
		def forward(dir: Direction, from: Pos, steps: Int): Pos = {
			if (steps <= 0) from
			else forward(dir, dir(from).get, steps - 1)
		}

		def checkPath(from: Pos, to: Pos, steps: Int) = {
			assert(!from.findDir(to).isEmpty && forward(from.findDir(to).get, from, steps) == to)
		}

		assert(A2.findDir(D7) === None)

		checkPath(D4, F6, 2)
		checkPath(D4, G4, 3)
		checkPath(D4, F2, 2)
		checkPath(D4, D3, 1)

		checkPath(D4, B2, 2)
		checkPath(D4, A4, 3)
		checkPath(D4, B6, 2)
		checkPath(D4, D6, 2)
	}

	test("index by key") {
		assert(Pos.posAt("a1") === Some(A1))
		assert(Pos.posAt("q9") === None)
	}

}
