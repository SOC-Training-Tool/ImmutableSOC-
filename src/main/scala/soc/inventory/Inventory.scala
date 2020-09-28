package soc.inventory

import soc.inventory.developmentCard._
import soc.inventory.developmentCard.DevelopmentCardSet._
import soc.inventory.resources._
import soc.inventory.resources.ResourceSet.{ResourceSet, Resources}

trait Inventory[II <: InventoryItem, T <: Inventory[II, T]] { self: T =>

  type UpdateRes
  type PUBLIC <: PublicInfoInventory[II, PUBLIC]

  val numCards: Int
  def canSpend(resSet: CatanSet[II, Int]): Boolean
  def updateResources(position: Int, update: UpdateRes): T
  def toPublicInventory: PUBLIC





//  type UpdateDev
//
//  val playedDevCards: DevelopmentCardSpecificationSet
//  val numUnplayedDevCards: Int
//
//  val pointCountIfKnown: Option[Int]
//
//  def canPlayCard(card: DevelopmentCard, turn: Int): Boolean
//
//  def endTurn: T
//
//  def updateDevelopmentCard(turn: Int, position: Int, update: UpdateDev): T

  //def toPublicInfo: PublicInfo
}

//object Inventory {
//  type PerfectInfo = PerfectInfoInventory
//  type ProbableInfo = ProbableInfoInventory
//  type PublicInfo = PublicInfoInventory
//}
//
//

trait PerfectInfoInventory[II <: InventoryItem, T <: PerfectInfoInventory[II, T]] extends Inventory [II, T]  { self: T =>

  override type UpdateRes = List[SOCTransactions[II]]

  val itemSet: CatanSet[II, Int]
  def updateSet(itemSet: CatanSet[II, Int]): T

  override def canSpend(resSet: CatanSet[II, Int]): Boolean = itemSet.contains(resSet)

  override val numCards: Int = itemSet.getTotal

  def updateResources(position: Int, transactions: List[SOCTransactions[II]]): T = updateSet(transactions.foldLeft(itemSet){
      case (newSet, Gain(`position`, set)) => newSet.add(set)
      case (newSet, Lose(`position`, set)) => newSet.subtract(set)
      case (newSet, Steal(`position`, _, Some(set))) => newSet.add(set)
      case (newSet, Steal(_, `position`, Some(set))) => newSet.subtract(set)
      case (newSet, _) => newSet
  })
}
case class PerfectInfoInventoryImpl[II <: InventoryItem](itemSet: CatanSet[II, Int]) extends PerfectInfoInventory[II, PerfectInfoInventoryImpl[II]] {
  override type PUBLIC = PublicInfoInventoryImpl[II]
  override def updateSet(itemSet: CatanSet[II, Int]): PerfectInfoInventoryImpl[II] = copy(itemSet = itemSet)
  override def toPublicInventory: PUBLIC = PublicInfoInventoryImpl(numCards)
}

trait PublicInfoInventory[II <: InventoryItem, T <: PublicInfoInventory[II, T]] extends Inventory[II, T] { self: T =>
  override type UpdateRes = List[SOCTransactions[II]]
  override def canSpend(resSet: CatanSet[II, Int]): Boolean = numCards > resSet.getTotal

  def updateNumCards(numCards: Int): T

  override def updateResources(position: Int, transactions: List[SOCTransactions[II]]): T = updateNumCards(transactions.foldLeft(numCards) {
    case (num, Gain(`position`, set)) => num + set.getTotal
    case (num, Lose(`position`, set)) => num - set.getTotal
    case (num, Steal(`position`, _, _)) => num + 1
    case (num, Steal(_, `position`, _)) => num - 1
    case (num, _) => num
  })
}
case class PublicInfoInventoryImpl[II <: InventoryItem](numCards: Int) extends PublicInfoInventory[II, PublicInfoInventoryImpl[II]] {
  override type PUBLIC = PublicInfoInventoryImpl[II]
  override def updateNumCards(numCards: Int): PublicInfoInventoryImpl[II] = copy(numCards)
  override def toPublicInventory: PUBLIC = this
}



//case class PerfectInfoInventory(
//  resourceSet: Resources = ResourceSet.empty,
//  developmentCards: DevelopmentCardSpecificationSet = DevelopmentCardSpecificationSet()
//) extends Inventory[PerfectInfo] {
//
//  type UpdateRes = List[SOCTransactions]
//  type UpdateDev = DevCardTransaction
//
//  override def updateResources(position: Int, transactions: List[SOCTransactions]): PerfectInfoInventory = {
//    val res = transactions.foldLeft(resourceSet){
//      case (newSet, Gain(`position`, set)) => newSet.add(set)
//      case (newSet, Lose(`position`, set)) => newSet.subtract(set)
//      case (newSet, Steal(`position`, _, Some(set))) => newSet.add(set)
//      case (newSet, Steal(_, `position`, Some(set))) => newSet.subtract(set)
//      case (newSet, _) => newSet
//    }
//    copy(resourceSet = res)
//  }
//
//  override def updateDevelopmentCard(turn: Int, position:Int, transaction: DevCardTransaction): PerfectInfoInventory = {
//    transaction match {
//      case BuyDevelopmentCard(`position`, Some(card)) =>
//        copy (developmentCards = developmentCards.buyCard(turn, card))
//      case PlayDevelopmentCard(`position`, card) =>
//        copy (
//          developmentCards = developmentCards.playCard(turn, card)
//        )
//      case _ => copy()
//    }
//
//  }
//
//  override def endTurn: PerfectInfoInventory = copy()
//
//  override val numUnplayedDevCards: Int = developmentCards.filterUnPlayed.getTotal
//  override val numCards: Int = resourceSet.getTotal
//
//  override def canSpend(resSet: Resources): Boolean = resourceSet.contains(resSet)
//  override def canPlayCard(card: DevelopmentCard, turn: Int): Boolean = developmentCards.canPlayCardOnTurn(card, turn)
//
//  override def toPublicInfo: PublicInfo = PublicInfoInventory(playedDevCards, numCards, numUnplayedDevCards)
//
//  override val playedDevCards: DevelopmentCardSpecificationSet = developmentCards.filterPlayed
//  override val pointCountIfKnown: Option[Int] = Some(developmentCards.getAmount(CatanPoint))
//}

//case class PublicInfoInventory(
//  playedDevCards: DevelopmentCardSpecificationSet = DevelopmentCardSpecificationSet(),
//  numCards: Int = 0,
//  numUnplayedDevCards: Int = 0) extends Inventory[PublicInfo] {
//
//  type UpdateRes = List[SOCTransactions]
//  type UpdateDev = DevCardTransaction
//
//  override def updateResources(position: Int, transactions: List[SOCTransactions]): PublicInfoInventory = {
//    val numCrds = transactions.foldLeft(numCards) {
//      case (num, Gain(`position`, set)) => num + set.getTotal
//      case (num, Lose(`position`, set)) => num - set.getTotal
//      case (num, Steal(`position`, _, _)) => num + 1
//      case (num, Steal(_, `position`, _)) => num - 1
//      case (num, _) => num
//    }
//    copy(numCards = numCrds)
//  }
//
//  override def updateDevelopmentCard(turn: Int, position: Int, transaction: DevCardTransaction): PublicInfoInventory = {
//    transaction match {
//      case BuyDevelopmentCard(`position`, Some(card)) =>
//        copy(numUnplayedDevCards = numUnplayedDevCards + 1)
//      case PlayDevelopmentCard(`position`, card) =>
//        copy(
//          numUnplayedDevCards = numUnplayedDevCards - 1,
//          playedDevCards = playedDevCards.playCard(turn, card)
//        )
//      case _ => copy()
//    }
//
//  }
//
//  override def endTurn: PublicInfoInventory = copy()
//
//  override def canSpend(resSet: Resources): Boolean = numCards >= resSet.getTotal
//  override def canPlayCard(card: DevelopmentCard, turn: Int): Boolean = !playedDevCards.playedCardOnTurn(turn) && numUnplayedDevCards > 0
//
//  override def toPublicInfo: PublicInfo = this
//
//  override val pointCountIfKnown: Option[Int] = {
//    val points = playedDevCards.getAmount(CatanPoint)
//    if (points == 0) None else Some(points)
//
//  }
//
//}
//
//case class ProbableInfoInventory(
//  probableResourceSet: ProbableResourceSet = ProbableResourceSet.empty,
//  possibleResourceSets: Seq[Resources] = Nil,
//  knownDevCards: DevelopmentCardSpecificationSet = DevelopmentCardSpecificationSet(),
//  probableDevCards: DevelopmentCardSet[Double] = DevelopmentCardSet.empty[Double]
//) extends Inventory[ProbableInfo]  {
//
//  type UpdateRes = (ProbableResourceSet, Seq[Resources])
//  type UpdateDev = (DevelopmentCardSpecificationSet, DevelopmentCardSet[Double])
//
//  override val playedDevCards: DevelopmentCardSpecificationSet = knownDevCards.filterPlayed
//
//  override val numUnplayedDevCards: Int = probableDevCards.getTotal.toInt + knownDevCards.filterUnPlayed.getTotal
//  override val numCards: Int = probableResourceSet.getTotal
//
//  override def canSpend(resSet: Resources): Boolean = probableResourceSet.mightContain(resSet)
//  override def canPlayCard(card: DevelopmentCard, turn: Int): Boolean = knownDevCards.canPlayCardOnTurn(card, turn) || probableDevCards.contains(card)
//
//  override def updateResources(position: Int, update: UpdateRes): ProbableInfoInventory = copy(probableResourceSet = update._1, possibleResourceSets = update._2)
//
//  override def endTurn: ProbableInfoInventory = copy()
//
//  override def updateDevelopmentCard(turn: Int, position: Int, update: (DevelopmentCardSpecificationSet, DevelopmentCardSet[Double])): ProbableInfoInventory = {
//    val (known, probable) = update
//    copy(
//      knownDevCards = known,
//      probableDevCards = probable
//    )
//  }
//
//  override def toPublicInfo: PublicInfo = PublicInfoInventory(playedDevCards, numCards, numUnplayedDevCards)
//
//  override val pointCountIfKnown: Option[Int] = Some(knownDevCards.getAmount(CatanPoint))
//}