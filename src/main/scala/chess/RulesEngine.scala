package chess

import scala.annotation.tailrec
import chess._
import Pos._


class RulesEngine(val state: State) {

	lazy val nextMoves: List[Move] = legalMoves
	lazy val gameStatus: Option[Status] = computeStatus

	val currentBoard: Board = state.board
	val currentHistory: History = state.history
	val sideToPlay: Color = state.sideToPlay
	val opponentSide: Color = !sideToPlay

	val kingPos: Option[Pos] = currentBoard.find((pos, piece) => piece.role == King && piece.color == sideToPlay)

	val kingInCheck: Boolean = kingThreatened

	def computeStatus: Option[Status] = {
		if (!nextMoves.isEmpty) None
		else if (kingInCheck) Some(Mate)
		else Some(Draw)
	}

	def legalMoves: List[Move] = {
		def rec(pos: Pos, piece: Piece): List[Move] = {
			if (piece is opponentSide) Nil
			else piece.role match {
				case Bishop => longRange(piece, pos, Bishop.dirs)
				case Knight => shortRange(piece, pos, Knight.dirs)
				case Queen => longRange(piece, pos, Queen.dirs)
				case Rook => longRange(piece, pos, Rook.dirs)
				case Pawn => pawnMoves(piece, pos)
				case King => shortRange(piece, pos, King.dirs) ++ castle(piece, pos)
			}
		}

		currentBoard.flatMap(rec).filter(move => !kingThreatened(move))
	}

	def longRange(piece: Piece, pos: Pos, dirs: Directions): List[Move] = {
		val nextHistory = {
			if (piece.role == Rook) currentHistory.rookMoved(sideToPlay, pos)
			else currentHistory
		}

		@tailrec
		def rec(from: Pos, dir: Direction, list: List[Move] = Nil): List[Move] = dir(from) match {
			case Some(to) => {
				if (!currentBoard.contains(to)) {
					currentBoard.move(pos, to).map(after => Move(state, State(after, nextHistory, opponentSide), piece, pos, to)) match {
						case Some(move) => rec(to, dir, move :: list)
						case _ => list
					}

				} else if (currentBoard.color(to).get == opponentSide) {
					currentBoard.take(pos, to).map(after => Move(state, State(after, nextHistory, opponentSide), piece, pos, to)) match {
						case Some(move) => move :: list
						case _ => list
					}

				} else {
					list
				}
			}
			case _ => list
		}

		dirs flatMap { dir => rec(pos, dir) }
	}

	def shortRange(piece: Piece, pos: Pos, dirs: Directions): List[Move] = {
		val nextHistory = {
			if (piece.role == King) currentHistory.kingMoved(sideToPlay)
			else currentHistory
		}

		dirs flatMap { dir =>
			dir(pos) match {
				case Some(to) => {
					if (!currentBoard.contains(to)) {
						currentBoard.move(pos, to).map(after => Move(state, State(after, nextHistory, opponentSide), piece, pos, to))

					} else if (currentBoard.color(to).get == opponentSide) {
						currentBoard.take(pos, to).map(after => Move(state, State(after, nextHistory, opponentSide), piece, pos, to))

					} else {
						Nil
					}
				}
				case _ => Nil
			}
		}
	}

	def firstPawnLine(pos: Pos): Boolean = {
		(sideToPlay == White && pos.y == 2) || (sideToPlay == Black && pos.y == 7)
	}

	def lastPawnLine(pos: Pos): Boolean = {
		(sideToPlay == White && pos.y == 8) || (sideToPlay == Black && pos.y == 1)
	}

	def pawnMoves(piece: Piece, pos: Pos): List[Move] = {
		def promote(dest: Pos, board: Board): Board = {
			if (!lastPawnLine(dest)) board
			else board.replace(Piece(sideToPlay, Queen), dest).get
		}

		val moveBy1: Option[Move] = piece.pawnMovingDir(pos) match {
			case Some(to) if !currentBoard.contains(to) =>
				currentBoard.move(pos, to) map { after =>
					Move(state, State(promote(to, after), currentHistory.resetEnPassant, opponentSide), piece, pos, to)
				}
			case _ => None
		}

		val moveBy2: Option[Move] = {
			if (!moveBy1.isEmpty && firstPawnLine(pos)) {
				piece.pawnMovingDir(moveBy1.get.dest) match {
					case Some(to) if !currentBoard.contains(to) =>
						currentBoard.move(pos, to) map { after =>
							Move(state, State(after, currentHistory.pawnMovedBy2(sideToPlay, pos.x), opponentSide), piece, pos, to)
						}
					case _ => None
				}
			} else None
		}

		def takeCross(dir: Direction): Option[Move] = dir(pos) match {
			case Some(to) if currentBoard.contains(to) && (currentBoard.color(to).get == opponentSide) =>
				currentBoard.take(pos, to) map { after =>
					Move(state, State(promote(to, after), currentHistory.resetEnPassant, opponentSide), piece, pos, to)
				}
			case _ => None
		}

		val takeEnPassant: Option[Move] = {
			if (currentHistory.allowed(piece.color, pos)) {
				val taken = Pos.posAt(currentHistory.enPassant.get.x, if (piece.color == White) 5 else 4)
				currentBoard.take(pos, currentHistory.enPassant.get, taken.get) map { after =>
					Move(state, State(after, currentHistory.resetEnPassant, opponentSide), piece, pos, currentHistory.enPassant.get)
				}
			} else None
		}

		(moveBy1 :: moveBy2 :: takeEnPassant :: (piece.pawnTakingDirs map takeCross)).flatten
	}

	def castle(piece: Piece, pos: Pos): List[Move] = {
		val shortKingDest = if (sideToPlay == White) G1 else G8
		val longKingDest = if (sideToPlay == White) C1 else C8

		val notShortAllowed = !currentHistory.allowed(sideToPlay, false) || currentBoard.contains(shortKingDest)
		val notLongAllowed = !currentHistory.allowed(sideToPlay, true) || currentBoard.contains(longKingDest)

		def stopCheck(ref: Pos): Boolean = {
			currentBoard.contains(ref) || posThreatened(currentBoard, ref)
		}

		def stopContains(ref: Pos): Boolean = currentBoard.contains(ref)

		def shortCastle: Option[Move] = {
			if (notShortAllowed) {
				None
			} else {
				if (Pos.checkAll(pos, shortKingDest, stopCheck)) {
					val rookOrigin = if (sideToPlay == White) H1 else H8
					val rookDest = if (sideToPlay == White) F1 else F8

					for (tmp <- currentBoard.move(pos, shortKingDest); after <- tmp.move(rookOrigin, rookDest))
					yield Move(state, State(after, currentHistory.kingMoved(sideToPlay), opponentSide), piece, pos, shortKingDest)

				} else None
			}
		}

		def longCastle: Option[Move] = {
			if (notLongAllowed) {
				None
			} else {
				val rookOrigin = if (sideToPlay == White) A1 else A8
				val rookDest = if (sideToPlay == White) D1 else D8

				if (Pos.checkAll(pos, longKingDest, stopCheck) && Pos.checkAll(rookOrigin, rookDest, stopContains)) {
					for (tmp <- currentBoard.move(pos, longKingDest); after <- tmp.move(rookOrigin, rookDest))
					yield Move(state, State(after, currentHistory.kingMoved(sideToPlay), opponentSide), piece, pos, longKingDest)

				} else None
			}
		}

		if (kingInCheck || (notShortAllowed && notLongAllowed)) Nil
		else List(shortCastle, longCastle).flatten
	}

	def kingThreatened: Boolean = {
		kingPos.exists(kp => posThreatened(currentBoard, kp))
	}

	def kingThreatened(move: Move): Boolean = {
		val nextKingPos = move.piece.role match {
			case King => Some(move.dest)
			case _ => kingPos
		}
		nextKingPos.exists(kp => posThreatened(move.after.board, kp))
	}

	def posThreatened(board: Board, ref: Pos): Boolean = {
		def pieceThreatenRef(pos: Pos, piece: Piece): Boolean = {
			(piece is opponentSide) && (piece.role match {
				case King => Pos.distance2(pos, ref) <= 2
				case Bishop => ref.onSameDiagonal(pos) && Pos.checkAll(pos, ref, pos => board.contains(pos))
				case Rook => ref.onSameLine(pos) && Pos.checkAll(pos, ref, pos => board.contains(pos))
				case Queen => (ref.onSameLine(pos) || ref.onSameDiagonal(pos)) && Pos.checkAll(pos, ref, pos => board.contains(pos))
				case Knight => Pos.distance2(pos, ref) == 5
				case Pawn => Pos.distance2(pos, ref) == 2 && piece.pawnTakingDirs.exists(dir => dir(pos) == Some(ref))
			})
		}

		board.exists(pieceThreatenRef)
	}

}
