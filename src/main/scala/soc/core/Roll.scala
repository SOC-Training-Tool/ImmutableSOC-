package soc.core

import soc.board.{BoardConfiguration, BoardRules}

case class Roll(number: Int)

object Roll {

  def apply(number: Int): Roll = new Roll(number)

  implicit class RollDetails[B <: BoardConfiguration](roll: Roll)(implicit boardRules: BoardRules[B]) {
    val dots: Int = (boardRules.robberRoll - 1) -  Math.abs(boardRules.robberRoll - roll.number)
    val prob: Double = dots.toDouble / boardRules.diceProbability.toDouble
  }

}