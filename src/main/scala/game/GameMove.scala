package game

trait GameMove {
  type R <: GameMoveResult

  def player: Int
}

trait GameMoveResult {
  type A <: GameMove

  def move: A
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, GameMoveResult.Move[A]]
}

object GameMoveResult {
  type Move[M <: GameMove] = GameMoveResult {type A = M}
}

trait PerfectInformationGameMove[A0 <: GameMove] extends GameMove with GameMoveResult {
  self: A0 =>
  type A = A0
  type R = A0

  def move: A = self
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, GameMoveResult.Move[A]] = playerIds.map(id => id -> this).toMap
}