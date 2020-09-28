package soc.dice

import scala.util.Random

trait Dice {
  val random: Random = new Random()

  val sides: Seq[Int]
  def getRoll(): Seq[Int] = sides.map(random.nextInt(_) + 1)
}

case class DiceRoll(roll: Seq[Int])


