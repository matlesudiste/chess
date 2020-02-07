package chess


case class Move(before: State, after: State, piece: Piece, origin: Pos, dest: Pos)
