package soc.actions

trait SOCMove {
  type R <: SOCMoveResult

  def player: Int
}

trait SOCMoveResult {
  type A <: SOCMove

  def move: A
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult.Move[A]]
}

object SOCMoveResult {
  type Move[M <: SOCMove] = SOCMoveResult {type A = M}
}

trait PerfectInformationSOCMove[A0 <: SOCMove] extends SOCMove with SOCMoveResult {
  self: A0 =>
  type A = A0
  type R = A0

  def move: A = self
  def getPerspectiveResults(playerIds: Seq[Int]): Map[Int, SOCMoveResult.Move[A]] = playerIds.map(id => id -> this).toMap
}