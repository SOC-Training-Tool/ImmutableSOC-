package soc.state

sealed trait GamePhase

object GamePhase extends Enumeration {
  type GamePhase = Value
  val Roll, BuyTradeOrEnd, Discard, MoveRobber, InitialPlacement, GameOver = Value
}