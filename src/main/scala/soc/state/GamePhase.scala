package soc.state

sealed trait GamePhase

object GamePhase {

  case object Roll extends GamePhase
  case object BuyTradeOrEnd extends GamePhase
  case object Discard extends GamePhase
  case object MoveRobber extends GamePhase
  case object InitialPlacement extends GamePhase
  case object GameOver extends GamePhase
}

