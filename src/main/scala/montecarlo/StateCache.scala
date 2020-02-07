package montecarlo

import com.github.blemale.scaffeine.{ Cache, Scaffeine }
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.{ SynchronizedMap, HashMap }
import scala.collection.convert.decorateAsScala._
import chess._


object StateCache {

	type StateExpand = (State => List[State])

	def noCache(state: State): List[State] = {
		new RulesEngine(state).nextMoves.map(_.after).toList
	}

	def caffeineCache(maxElements: Int, load: StateExpand = noCache): StateExpand = {
		val cache: Cache[Long, List[State]] = Scaffeine()
												.recordStats()
												.maximumSize(maxElements)
												.build[Long, List[State]]()

		def retrieve(state: State): List[State] = {
			val hash = state.hash
			val list = cache.getIfPresent(hash)
			if (!list.isEmpty) {
				list.get
			} else {
				val expanded = load(state)
				cache.put(hash, expanded)
				expanded
			}
		}

		retrieve
	}

	def scalaHashMap(load: StateExpand = noCache): StateExpand = {
		val cache = new HashMap[Long, List[State]] with SynchronizedMap[Long, List[State]]

		def retrieve(state: State): List[State] = {
			val hash = state.hash
			val list = cache.get(hash)
			if (!list.isEmpty) {
				list.get
			} else {
				val expanded = load(state)
				cache.put(hash, expanded)
				expanded
			}
		}

		retrieve
	}

	def javaConcurrentMap(load: StateExpand = noCache): StateExpand = {
		val cache = new ConcurrentHashMap[Long, List[State]]().asScala

		def retrieve(state: State): List[State] = {
			val hash = state.hash
			if (cache.contains(hash)) {
				cache.get(hash).get
			} else {
				val expanded = load(state)
				cache.put(hash, expanded)
				expanded
			}
		}

		retrieve
	}

}
