package soc.base.actions.developmentcards

import game._
import shapeless.{::, HNil}
import soc.base.state.ops._
import soc.base.state.{Bank, Turn}
import soc.base.{BuyDevelopmentCardMoveResult, PerfectInfoBuyDevelopmentCardMoveResult}
import soc.inventory.DevTransactions.{ImperfectInfoBuyCard, PerfectInfoBuyCard}
import soc.inventory.DevelopmentCardInventories.DevelopmentCardInventoriesOps
import soc.inventory.{BuyDevelopmentCard, DevelopmentCardInventories, ResourceInventories, Transactions}
import util.DependsOn

case class DevelopmentCardDeckSize(size: Int)

object BuyDevelopmentCardAction {

  private def action[M, Res, ResInv[_], Dev, DevTrans, DevInv[_]]
  (cost: InventorySet[Res, Int])(player: M => Int, transaction: M => DevTrans)
  (implicit res: ResourceInventories[Res, Transactions.PerfectInfo[Res], ResInv],
   dev: DevelopmentCardInventories[Dev, DevInv],
   buyDev: BuyDevelopmentCard[DevTrans, DevInv[Dev]]
  ): GameAction[M, DevInv[Dev] :: DevelopmentCardDeckSize :: ResInv[Res] :: Bank[Res] :: Turn :: HNil] = {
    type STATE = DevInv[Dev] :: DevelopmentCardDeckSize :: ResInv[Res] :: Bank[Res] :: Turn :: HNil
    GameAction[M, STATE] { case (move, state) =>
      implicit val turnDep = DependsOn[STATE, Turn :: HNil]
      implicit val invDep = DependsOn[STATE, Bank[Res] :: ResInv[Res] :: HNil]
      state.updateWith[DevInv[Dev], DevInv[Dev], STATE](_.buyCard[DevTrans](player(move), state.turn, transaction(move)))
        .updateWith[DevelopmentCardDeckSize, DevelopmentCardDeckSize, STATE](d => d.copy(d.size - 1))
        .payToBank(player(move), cost)
    }
  }

  def apply[Res, ResInv[_], Dev, DevInv[_]]
  (cost: InventorySet[Res, Int])
  (implicit res: ResourceInventories[Res, Transactions.PerfectInfo[Res], ResInv],
   dev: DevelopmentCardInventories[Dev, DevInv],
   buyDev: BuyDevelopmentCard[ImperfectInfoBuyCard[Dev], DevInv[Dev]]
  ): GameAction[BuyDevelopmentCardMoveResult[Dev], DevInv[Dev] :: DevelopmentCardDeckSize :: ResInv[Res] :: Bank[Res] :: Turn :: HNil] = {
    action[BuyDevelopmentCardMoveResult[Dev], Res, ResInv, Dev, ImperfectInfoBuyCard[Dev], DevInv](cost)(
      _.player,
      m => ImperfectInfoBuyCard[Dev](m.card))
  }

  def perfectInfo[Res, ResInv[_], Dev, DevInv[_]]
  (cost: InventorySet[Res, Int])
  (implicit res: ResourceInventories[Res, Transactions.PerfectInfo[Res], ResInv],
   dev: DevelopmentCardInventories[Dev, DevInv],
   buyDev: BuyDevelopmentCard[PerfectInfoBuyCard[Dev], DevInv[Dev]]
  ): GameAction[PerfectInfoBuyDevelopmentCardMoveResult[Dev], DevInv[Dev] :: DevelopmentCardDeckSize :: ResInv[Res] :: Bank[Res] :: Turn :: HNil] = {
    action[PerfectInfoBuyDevelopmentCardMoveResult[Dev], Res, ResInv, Dev, PerfectInfoBuyCard[Dev], DevInv](cost)(
      _.player,
      m => PerfectInfoBuyCard[Dev](m.card))
  }

  def initDevCardSize(size: Int) = new StateInitializer[DevelopmentCardDeckSize] {
    override def apply(): DevelopmentCardDeckSize = DevelopmentCardDeckSize(size)
  }
}
