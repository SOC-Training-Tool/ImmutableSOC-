package game

import game.GameMoveResult.{Aux}

trait GameMove {
  type R <: GameMoveResult

  def player: Int
}

trait GameMoveResult {
  type A <: GameMove

  def move: A
}

trait PerfectInfoMoveResult extends GameMoveResult {
  type ImperfectInfoMoveResult <: Aux[A]
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, ImperfectInfoMoveResult]

}

object GameMoveResult {
  type Aux[M <: GameMove] = GameMoveResult {type A = M}
  type PAux[M <: GameMove, R <: Aux[M]] = PerfectInfoMoveResult {type ImperfectInfoMoveResult = R}
}

trait PerfectInformationGameMove[A0 <: GameMove with PerfectInformationGameMove[A0]] extends GameMove with PerfectInfoMoveResult {
  self: A0 =>
  override type A = A0
  override type ImperfectInfoMoveResult = A0

  override def move: A = self
  override def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, ImperfectInfoMoveResult] = playerIds.map(id => id -> self).toMap
}