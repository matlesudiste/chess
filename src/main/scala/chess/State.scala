package chess


case class State(board: Board, history: History, sideToPlay: Color) {
	
	val hash: Long = ZobristHashChess.addState(board.hash, history, sideToPlay)

	override def hashCode: Int = hash.hashCode

}


object State {

	def init: State = State(Board.init, History(), White)

}
