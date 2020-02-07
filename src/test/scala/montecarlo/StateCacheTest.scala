package montecarlo

import org.scalatest.FunSuite
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger
import chess._
import StateCache._


class StateCacheTest extends FunSuite {

	val games = MasterGame.loadAllGames(MasterGame.smallNbOfGames)

	test("basic operations") {
		val noCacheCount = new AtomicInteger(0)

		def noCache(state: State): List[State] = {
       		noCacheCount.incrementAndGet
	       	StateCache.noCache(state)
		}

		val caches: List[StateExpand] = List(StateCache.caffeineCache(100, noCache),
											StateCache.scalaHashMap(noCache),
											StateCache.javaConcurrentMap(noCache))

		val state = State.init
		val ref = noCache(state).toSet
		noCacheCount.set(0)

		for (cache <- caches) assert(cache(state).toSet === ref)
		assert(noCacheCount.get === caches.size)

		for (cache <- caches) assert(cache(state).toSet === ref)
		assert(noCacheCount.get === caches.size)
	}

	def checkThreadSafe(cache: StateExpand, nbElements: Int) {
		val moves = Random.shuffle(games.flatMap(_.moves)).take(nbElements)

		def check: Unit = {
			val rand = Random
			for (i <- 0 to 100000) {
				val k = rand.nextInt(nbElements)
				assert(cache(moves(k).before).contains(moves(k).after))
			}
		}

		val tasks = Vector(check _, check _, check _, check _,
							check _, check _, check _, check _)
		
		val results = tasks.par.map(_()).seq
	}

	test("thread safe for caffeine") {
		val nbElements = 100
		val cache = StateCache.caffeineCache(2 * nbElements)

		checkThreadSafe(cache, nbElements)
	}

	test("thread safe for scala hash map") {
		val nbElements = 100
		val cache = StateCache.scalaHashMap()

		checkThreadSafe(cache, nbElements)
	}

	test("thread safe for java concurrent map") {
		val nbElements = 100
		val cache = StateCache.javaConcurrentMap()

		checkThreadSafe(cache, nbElements)
	}

	def computeCachePerfMinMax(root: State): (Double, Int) = {
		val noCacheCount = new AtomicInteger(0)
		val cacheTotalHit = new AtomicInteger(0)

		def noCache(state: State): List[State] = {
       		noCacheCount.incrementAndGet
       		StateCache.noCache(state)
		}

		val cache = StateCache.caffeineCache(1000000, noCache)

		def cacheWithStats(state: State): List[State] = {
			cacheTotalHit.incrementAndGet
			cache(state)
		}

		def rec(state: State, depth: Int): Unit = {
			if (depth > 0) {
				for (child <- cacheWithStats(state)) rec(child, depth - 1)
			}
		}

		rec(root, 4)

		val perf = (cacheTotalHit.get - noCacheCount.get).toDouble / cacheTotalHit.get * 100.0
		val total = cacheTotalHit.get

		(perf, total)
	}

	// RESULT : for depth 4, average improved perf is 42%
	test("cache hits for pure min max") {
		val statesInMiddle = games.flatMap(_.moves.drop(10).take(15))

		def randomRoot: State = statesInMiddle(Random.nextInt(statesInMiddle.size)).before

		val tasks = Vector(() => computeCachePerfMinMax(randomRoot),
							() => computeCachePerfMinMax(randomRoot),
							() => computeCachePerfMinMax(randomRoot),
							() => computeCachePerfMinMax(randomRoot),
							() => computeCachePerfMinMax(randomRoot),
							() => computeCachePerfMinMax(randomRoot),
							() => computeCachePerfMinMax(randomRoot),
							() => computeCachePerfMinMax(randomRoot))
		
		val results = tasks.par.map(_()).seq

		val perf = (results.map(_._1).sum / results.size).toInt
		val total = results.map(_._2).sum / 1000

		println("cache hits for pure min max : improve speed by " + perf + "% over " + total + "k total hits")
	}

}
