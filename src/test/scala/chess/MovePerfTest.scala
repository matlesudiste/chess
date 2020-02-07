package chess

import org.scalameter.api._
import org.scalameter.picklers.noPickler._


class MovePerfTest extends Bench.OnlineRegressionReport {
	
	val moves = MasterGame.loadAllGames(MasterGame.smallNbOfGames).flatMap(game => game.moves)

	performance of "***** PERF TEST CHESS *****" in {
		measure method "Average computation time of legal moves" in {
			using(Gen.unit("board")) in {
				Unit => for (move <- moves) yield new RulesEngine(move.after).legalMoves
			}
		}
	}

}
