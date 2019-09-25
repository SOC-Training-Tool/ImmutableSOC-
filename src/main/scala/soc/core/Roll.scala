package soc.core

case class Roll(number: Int) {
  val dots: Int = 6 - Math.abs(7 - number)
  val prob: Double = dots.toDouble / 36.0
}