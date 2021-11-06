package soc.moves2

trait GameOps[B, I, P, S] {

  def onRollDice(rollDiceResult: RollDiceResult): S
  def endTurn: S

}
