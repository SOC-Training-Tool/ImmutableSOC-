package soc.inventory

import Inventory.{NoInfo, PerfectInfo, ProbableInfo}
import soc.inventory.developmentCard._
import soc.inventory.developmentCard.DevelopmentCardSet._
import soc.inventory.resources._
import soc.inventory.resources.CatanResourceSet.Resources

trait Inventory[T <: Inventory[T]] { self: T =>

  type UpdateRes
  type UpdateDev

  val playedDevCards: DevelopmentCardSpecificationSet
  val numUnplayedDevCards: Int
  val numCards: Int
  def canBuild(resSet: Resources): Boolean
  def endTurn: T
  def updateResources(position: Int, update: UpdateRes): T
  def updateDevelopmentCard(turn: Int, position: Int, update: UpdateDev): T

  def toPublicInfo: NoInfo
}

case object Inventory {
  type PerfectInfo = PerfectInfoInventory
  type ProbableInfo = ProbableInfoInventory
  type NoInfo = NoInfoInventory
}


case class PerfectInfoInventory(
  resourceSet: Resources = CatanResourceSet.empty,
  developmentCards: DevelopmentCardSpecificationSet = DevelopmentCardSpecificationSet()
) extends Inventory[PerfectInfo] {

  type UpdateRes = List[SOCTransactions]
  type UpdateDev = DevCardTransaction

  override def updateResources(position: Int, transactions: List[SOCTransactions]): PerfectInfoInventory = {
    val res = transactions.foldLeft(resourceSet){
      case (newSet, Gain(`position`, set)) => newSet.add(set)
      case (newSet, Lose(`position`, set)) => newSet.subtract(set)
      case (newSet, Steal(`position`, _, Some(set))) => newSet.add(set)
      case (newSet, Steal(_, `position`, Some(set))) => newSet.subtract(set)
      case (newSet, _) => newSet
    }
    copy(resourceSet = res)
  }

  override def updateDevelopmentCard(turn: Int, position:Int, transaction: DevCardTransaction): PerfectInfoInventory = {
    transaction match {
      case BuyDevelopmentCard(`position`, Some(card)) =>
        copy (developmentCards = developmentCards.buyCard(turn, card))
      case PlayDevelopmentCard(`position`, card) =>
        copy (
          developmentCards = developmentCards.playCard(turn, card)
        )
      case _ => copy()
    }

  }

  override def endTurn: PerfectInfoInventory = copy()

  override val numUnplayedDevCards: Int = developmentCards.filterUnPlayed.getTotal
  override val numCards: Int = resourceSet.getTotal

  override def canBuild(resSet: Resources): Boolean = resourceSet.contains(resSet)

  override def toPublicInfo: NoInfo = NoInfoInventory(playedDevCards, numCards, numUnplayedDevCards)

  override val playedDevCards: DevelopmentCardSpecificationSet = developmentCards.filterPlayed
}

case class NoInfoInventory(
  playedDevCards: DevelopmentCardSpecificationSet = DevelopmentCardSpecificationSet(),
  numCards: Int = 0,
  numUnplayedDevCards: Int = 0) extends Inventory[NoInfo] {

  type UpdateRes = List[SOCTransactions]
  type UpdateDev = DevCardTransaction

  override def updateResources(position: Int, transactions: List[SOCTransactions]): NoInfoInventory = {
    val numCrds = transactions.foldLeft(numCards) {
      case (num, Gain(`position`, set)) => num + set.getTotal
      case (num, Lose(`position`, set)) => num - set.getTotal
      case (num, Steal(`position`, _, _)) => num + 1
      case (num, Steal(_, `position`, _)) => num - 1
      case (num, _) => num
    }
    copy(numCards = numCrds)
  }

  override def updateDevelopmentCard(turn: Int, position: Int, transaction: DevCardTransaction): NoInfoInventory = {
    transaction match {
      case BuyDevelopmentCard(`position`, Some(card)) =>
        copy(numUnplayedDevCards = numUnplayedDevCards + 1)
      case PlayDevelopmentCard(`position`, card) =>
        copy(
          numUnplayedDevCards = numUnplayedDevCards - 1,
          playedDevCards = playedDevCards.playCard(turn, card)
        )
      case _ => copy()
    }

  }

  override def endTurn: NoInfoInventory = copy()

  override def canBuild(resSet: Resources): Boolean = true

  override def toPublicInfo: NoInfo = this
}

case class ProbableInfoInventory(
  playedDevCards: DevelopmentCardSpecificationSet = DevelopmentCardSpecificationSet(),
  probableResourceSet: ProbableResourceSet = ProbableResourceSet.empty,
  knownUnplayedDevCards: PlayedInventory=  DevelopmentCardSet.empty,
  probableDevCards: DevelopmentCardSet[Double] = DevelopmentCardSet.empty
) extends Inventory[ProbableInfo]  {

  type UpdateRes = ProbableResourceSet
  type UpdateDev = (DevelopmentCardSpecificationSet, PlayedInventory, DevelopmentCardSet[Double])

  override val numUnplayedDevCards: Int = probableDevCards.getTotal.toInt + knownUnplayedDevCards.getTotal
  override val numCards: Int = probableResourceSet.getTotal

  override def canBuild(resSet: Resources): Boolean = probableResourceSet.mightContain(resSet)

  override def updateResources(position: Int, probableSet: ProbableResourceSet): ProbableInfoInventory = copy(probableResourceSet = probableSet)

  override def endTurn: ProbableInfoInventory = copy()

  override def updateDevelopmentCard(turn: Int, position: Int, update: (DevelopmentCardSpecificationSet, PlayedInventory, DevelopmentCardSet[Double])): ProbableInfoInventory = {
    val (played, known, probable) = update
    copy(
      playedDevCards = played,
      knownUnplayedDevCards = known,
      probableDevCards = probable
    )
  }

  override def toPublicInfo: NoInfo = NoInfoInventory(playedDevCards, numCards, numUnplayedDevCards)
}