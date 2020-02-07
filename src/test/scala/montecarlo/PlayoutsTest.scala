package montecarlo

import org.scalatest.FunSuite
import java.io.FileWriter
import java.io.File
import java.time.LocalDate
import chess._
import StateCache._


class PlayoutsTest extends FunSuite {

	val bestCacheSize = 40000

	def memoryUsed: Long = (Runtime.getRuntime.totalMemory - Runtime.getRuntime.freeMemory) / (1024 * 1024)

	def endWithMate(moves: List[Move]): Boolean = {
		new RulesEngine(moves.last.after).gameStatus == Some(Mate)
	}

	def mateRatio(stats: List[(Option[Color], Int)]): Double = {
		stats.filter(!_._1.isEmpty).map(_._2).sum.toDouble / stats.map(_._2).sum * 100.0
	}

	val games = MasterGame.loadAllGames(MasterGame.smallNbOfGames)
	val mateGames = games.filter(game => endWithMate(game.moves))

	def generateStats(cacheSize: Int, maxPlyCount: Int, states: Seq[State]) = {
		System.gc
		val cache = StateCache.caffeineCache(cacheSize)

		val startTime = System.nanoTime

		val playout = new Playouts(cache, maxPlyCount)

		val ratio = for (state <- states) yield mateRatio(playout.launch(state))

		val elapsedTime = System.nanoTime - startTime

		val averageMateRatio = ratio.sum / ratio.size
		val totalNbOfRollOut = ratio.size * playout.nbOfThrows
		val speed = (averageMateRatio * totalNbOfRollOut * 1000000000.0) / elapsedTime

		val message = "max ply count : " + maxPlyCount + " | " +
						"average ratio for " + (totalNbOfRollOut / 1000) + "k" +
						" rollouts ending with mate : " + f"$averageMateRatio%2.1f" + "%" +
						" in " + (elapsedTime / 1000000000.0).toInt + " sec" +
						" => " + speed.toInt + " mate/sec"

		println(message)

		val metrics = List(("cacheSize", cacheSize), ("maxPlyCount", maxPlyCount), ("totalNbOfRollOut", totalNbOfRollOut),
							("averageMateRatio", averageMateRatio), ("speed", speed.toInt), ("elapsedTime", elapsedTime),
							("message", message))
		addToCsv(metrics)
	}

	def addToCsv(metrics: List[(String, Any)]) = {
		val fileName = "./mc_results.csv"

		val header = ("date" :: metrics.map(_._1)).mkString(";")
		val line = (LocalDate.now :: metrics.map(_._2)).mkString(";")

		val alreadyExists = new File(fileName).exists

		val fw = new FileWriter(fileName, true)
		try {
			if (!alreadyExists) fw.write(header + "\n")
			fw.write(line + "\n")
		}
		finally fw.close
	}

	test("follow complete games ending with a mate") {
		System.gc
		println("memory used before MC : " + memoryUsed + " Mo")

		val inputs = for (cacheSize <- List(40000); maxPlyCount <- List(270)) yield (cacheSize, maxPlyCount)
		val states = for (game <- mateGames.take(1); i <- 0 until game.moves.size by 2) yield game.moves(i).before

		for (input <- inputs) generateStats(input._1, input._2, states)

		println("memory used after MC : " + memoryUsed + " Mo")

		System.gc
		println("memory used after gc : " + memoryUsed + " Mo")
	}

}
