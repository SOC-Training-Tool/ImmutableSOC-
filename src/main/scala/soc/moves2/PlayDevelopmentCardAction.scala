package soc.moves2

import soc.inventory.DevelopmentCard

trait PlayDevelopmentCardMove[A <: PlayDevelopmentCardMove[A]] extends SOCMove[PlayDevelopmentCardMove[A]]
trait PlayDevelopmentCardAction[A <: PlayDevelopmentCardMove[A]] extends GameAction[A] {
  val cardsInDeck: Int
  val cardType: DevelopmentCard
}
