package state

case class TurnState(
  canPlayDevCard: Boolean = true,
  canRollDice: Boolean = true
)