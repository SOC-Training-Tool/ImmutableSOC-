package soc.moves2.developmentcard

import soc.inventory.InventoryHelper.{PerfectInfoInv, PublicInfoInv}
import soc.inventory.developmentCard.{BuyDevelopmentCard, DevCardTransaction, DevelopmentCardSpecificationSet, PlayDevelopmentCard}
import soc.inventory.resources.SOCTransactions
import soc.inventory.{CatanSet, DevelopmentCard, Inventory, InventoryHelper, InventoryHelperFactory, InventoryItem, PerfectInfoInventory, PerfectInfoInventoryImpl, PublicInfoInventory, PublicInfoInventoryImpl, SimpleInventoryHelper}
import soc.moves2.developmentcard.SimpleDevInventoryHelper.SimpleInventory


object DevelopmentCardInventory {
  type PerfectInfoDevInvHelper[II <: InventoryItem] = SimpleDevInventoryHelper[II, PerfectInfoDevInv[II]]
  type PublicInfoDevInvHelper[II <: InventoryItem] = SimpleDevInventoryHelper[II, PublicInfoDevInv[II]]

  type PerfectInfoDevInv[II <: InventoryItem] = PerfectInfoDevelopmentCardInventory[II]
  type PublicInfoDevInv[II <: InventoryItem] = PublicInfoDevelopmentCardInventory[II]

  implicit def publicInfoDevInvFactory[II <: InventoryItem](implicit publicInfoFactory: InventoryHelperFactory[II, PublicInfoInv[II]]): InventoryHelperFactory[II, PublicInfoDevInvHelper[II]] = { players =>
    SimpleDevInventoryHelper[II, PublicInfoDevelopmentCardInventory[II]](
      SimpleInventoryHelper[II, PublicInfoDevelopmentCardInventory[II], PublicInfoDevelopmentCardInventory[II]](
        publicInfoFactory.create(players).playerInventories.map { case(p, i) =>
          p -> PublicInfoDevelopmentCardInventory(i, DevelopmentCardSpecificationSet(), 0)
        }
      )
    )
  }

  implicit def perfectInfoDevInvFactory[II <: InventoryItem](implicit perfectInfoFactory: InventoryHelperFactory[II, PerfectInfoInv[II]]): InventoryHelperFactory[II, PerfectInfoDevInvHelper[II]] = { players =>
    SimpleDevInventoryHelper[II, PerfectInfoDevelopmentCardInventory[II]](
      SimpleInventoryHelper[II, PublicInfoDevelopmentCardInventory[II], PerfectInfoDevelopmentCardInventory[II]](
        perfectInfoFactory.create(players).playerInventories.map { case (p, i) =>
          p -> PerfectInfoDevelopmentCardInventory(i, DevelopmentCardSpecificationSet())
        }
      )
    )
  }

}

trait DevelopmentCardInventoryHelper[II <: InventoryItem, DI <: DevelopmentCardInventoryHelper[II, DI]] extends InventoryHelper[II, DI] { self: DI =>
  override type INV <: DevelopmentCardInventory[II, INV]

  def canPlayCard(player: Int, card: DevelopmentCard, turn: Int) = playerInventories.get(player).fold[Boolean](false)(_.canPlayCard(card, turn))

  def buyDevelopmentCard(position: Int, turn: Int, card: Option[DevelopmentCard]): DI

  def playDevelopmentCard(position: Int, turn: Int, card: DevelopmentCard): DI
}

object SimpleDevInventoryHelper {
  type SimpleInventory[II <: InventoryItem, I <: DevelopmentCardInventory[II, I]] = DevelopmentCardInventory[II, I] {
    type UpdateRes = List[SOCTransactions[II]]
    type UpdateDev = DevCardTransaction
    type PUBLIC = PublicInfoDevelopmentCardInventory[II]
  }
}

case class SimpleDevInventoryHelper[II <: InventoryItem, I <: SimpleInventory[II, I]](
  baseSimpleInvHelper: SimpleInventoryHelper[II, PublicInfoDevelopmentCardInventory[II], I]
) extends DevelopmentCardInventoryHelper[II, SimpleDevInventoryHelper[II, I]] {
  override type INV = I

  override def buyDevelopmentCard(position: Int, turn: Int, card: Option[DevelopmentCard]): SimpleDevInventoryHelper[II, I] = copy {
    baseSimpleInvHelper.copy( playerInventories.map {
      case (`position`, dInv) => position -> dInv.updateDevelopmentCards(turn, position, BuyDevelopmentCard(position, card))
      case (p, dInv) => p -> dInv
    })
  }
  override def playDevelopmentCard(position: Int, turn: Int, card: DevelopmentCard): SimpleDevInventoryHelper[II, I] = copy {
    baseSimpleInvHelper.copy( playerInventories.map {
      case (`position`, dInv) => position -> dInv.updateDevelopmentCards(turn, position, PlayDevelopmentCard(position, card))
      case (p, dInv) => p -> dInv
    })
  }

  override type PUBLIC = SimpleDevInventoryHelper[II, PublicInfoDevelopmentCardInventory[II]]

  override def playerInventories: Map[Int, INV] = baseSimpleInvHelper.playerInventories
  override def updateInventory(transactions: List[SOCTransactions[II]]): SimpleDevInventoryHelper[II, I] = copy(baseSimpleInvHelper = baseSimpleInvHelper.updateInventory(transactions))

  override def toPublic: PUBLIC = SimpleDevInventoryHelper[II, PublicInfoDevelopmentCardInventory[II]](baseSimpleInvHelper.toPublic)
}

trait DevelopmentCardInventory[II <: InventoryItem, T <: DevelopmentCardInventory[II, T]] extends Inventory[II, T]{ self: T =>
  type PUBLIC = PublicInfoDevelopmentCardInventory[II]
  type UpdateDev

  val playedDevCards: DevelopmentCardSpecificationSet
  val numUnplayedDevCards: Int

  def canPlayCard(card: DevelopmentCard, turn: Int): Boolean
  def updateDevelopmentCards(turn: Int, position: Int, update: UpdateDev): T

  def toPublicInventory: PUBLIC

  type INV <: Inventory[II, INV] { type UpdateRes = T#UpdateRes}

  def updateBaseInventory(inv: INV): T
  def baseInventory: INV

  override val numCards: Int = baseInventory.numCards
  override def canSpend(resSet: CatanSet[II, Int]): Boolean = baseInventory.canSpend(resSet)
  override def updateResources(position: Int, update: UpdateRes): T = updateBaseInventory(baseInventory.updateResources(position, update.asInstanceOf[INV#UpdateRes]))

}

case class PerfectInfoDevelopmentCardInventory[II <: InventoryItem](
  baseInventory: PerfectInfoInventoryImpl[II],
  developmentCards: DevelopmentCardSpecificationSet = DevelopmentCardSpecificationSet()
) extends DevelopmentCardInventory[II, PerfectInfoDevelopmentCardInventory[II]] with PerfectInfoInventory[II, PerfectInfoDevelopmentCardInventory[II]]{

  override type UpdateDev = DevCardTransaction
  type INV = PerfectInfoInventoryImpl[II]
  override val playedDevCards = developmentCards.filterPlayed
  val unplayedDevelopmentCards = developmentCards.filterUnPlayed
  override val numUnplayedDevCards: Int = unplayedDevelopmentCards.length

  override def canPlayCard(card: DevelopmentCard, turn: Int): Boolean = {
    !playedDevCards.playedCardOnTurn(turn) && unplayedDevelopmentCards.canPlayCardOnTurn(card, turn)
  }

  override def updateDevelopmentCards(turn: Int, position:Int, transaction: DevCardTransaction): PerfectInfoDevelopmentCardInventory[II] = {
    transaction match {
      case BuyDevelopmentCard(`position`, Some(card)) => copy(developmentCards = developmentCards.buyCard(turn, card))
      case PlayDevelopmentCard(`position`, card) => copy(developmentCards = developmentCards.playCard(turn, card))
      case _ => this
    }
  }

  override def toPublicInventory: PUBLIC = PublicInfoDevelopmentCardInventory[II](
    baseInventory.toPublicInventory,
    playedDevCards,
    numUnplayedDevCards,
    developmentCards.cards.groupBy(_.turnPurchased.get).view.mapValues(_.length).toMap
  )

  override def updateBaseInventory(inv: PerfectInfoInventoryImpl[II]): PerfectInfoDevelopmentCardInventory[II] = copy(baseInventory = inv)

  override val itemSet: CatanSet[II, Int] = baseInventory.itemSet
  override def updateSet(itemSet: CatanSet[II, Int]): PerfectInfoDevelopmentCardInventory[II] = copy(baseInventory = baseInventory.updateSet(itemSet))
}

case class PublicInfoDevelopmentCardInventory[II <: InventoryItem](
  baseInventory: PublicInfoInventoryImpl[II],
  playedDevCards: DevelopmentCardSpecificationSet = DevelopmentCardSpecificationSet(),
  numUnplayedDevCards: Int = 0,
  private val dcardBuyTurn: Map[Int, Int] = Map.empty
) extends DevelopmentCardInventory[II, PublicInfoDevelopmentCardInventory[II]] with PublicInfoInventory[II, PublicInfoDevelopmentCardInventory[II]]{

  override type UpdateDev = DevCardTransaction
  override type INV = PublicInfoInventoryImpl[II]

  override def canPlayCard(card: DevelopmentCard, turn: Int): Boolean = {
    !playedDevCards.playedCardOnTurn(turn) && numUnplayedDevCards > dcardBuyTurn.getOrElse(turn, 0)
  }

  override def updateDevelopmentCards(turn: Int, position:Int, transaction: DevCardTransaction): PublicInfoDevelopmentCardInventory[II] = {
    transaction match {
      case BuyDevelopmentCard(`position`, Some(_)) =>
        copy(numUnplayedDevCards = numUnplayedDevCards + 1, dcardBuyTurn =  dcardBuyTurn + (turn -> (dcardBuyTurn.getOrElse(turn, 0) + 1)))
      case PlayDevelopmentCard(`position`, card) => copy(playedDevCards = playedDevCards.playCard(turn, card), numUnplayedDevCards = numUnplayedDevCards - 1)
      case _ => this
    }
  }

  override def updateBaseInventory(inv: PublicInfoInventoryImpl[II]): PublicInfoDevelopmentCardInventory[II] = copy(baseInventory = inv)

  override def updateNumCards(numCards: Int): PublicInfoDevelopmentCardInventory[II] = copy(baseInventory = baseInventory.updateNumCards(numCards))

  override def toPublicInventory: PublicInfoDevelopmentCardInventory[II] = this
}