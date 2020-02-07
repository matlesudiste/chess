package montecarlo

import scala.concurrent.forkjoin.ForkJoinPool
import scala.collection.parallel.immutable.ParSeq
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec
import chess._
import StateCache._


class Playouts(expand: StateExpand, val maxPlyCount: Int = 270, val nbOfThrows: Int = 1000) {

	val parList: ParSeq[Int] = {
		val cpuCores = Runtime.getRuntime.availableProcessors()
		val forkNum = if (cpuCores > 2) 2 * cpuCores else 2

		val list = (0 until nbOfThrows).par
		list.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(forkNum))

		list
	}

	def nextInt(bound: Int): Int = ThreadLocalRandom.current.nextInt(bound)

	def countPieces(board: Board): Int = {
		board.pieces.foldLeft(0) { (acc, elt) => if (!elt.isEmpty) acc + 1 else acc }
	}

	def winnerIf5(board: Board): Option[Color] = {
		// TODO
		Some(White)
	}

	def launch(initial: State): List[(Option[Color], Int)] = {
		@tailrec
		def loop(state: State, plyCount: Int = 0): Option[Color] = {
			val list = expand(state)

			if (list.isEmpty) {
				val status = new RulesEngine(state).gameStatus
				if (status == Some(Mate)) Some(!state.sideToPlay)
				else None
			} else if (countPieces(state.board) <= 6) {
				winnerIf5(state.board)
			} else if (plyCount >= maxPlyCount) {
				None
			} else {
				val next = list(nextInt(list.size))
				loop(next, plyCount + 1)
			}
		}

		val results = parList.map(i => loop(initial))

		val whites = results.foldLeft(0) { (acc, elt) => if (!elt.isEmpty && elt.get == White) acc + 1 else acc }
		val blacks = results.foldLeft(0) { (acc, elt) => if (!elt.isEmpty && elt.get == Black) acc + 1 else acc }

		List((Some(White), whites), (Some(Black), blacks), (None, results.size - whites - blacks))
	}

}
