package soc.inventory

import game.{InventorySet, StateInitializer}
import shapeless.{:+:, ::, CNil, Coproduct, HNil, Poly2}
import soc.inventory.Transactions._
import util.opext.Embedder

object Transactions {

  case class Gain[II](player: Int, set: InventorySet[II, Int])

  case class Lose[II](player: Int, set: InventorySet[II, Int])

  case class ImperfectInfoExchange[II](from: Int, to: Int, exchange: Option[II])

  type PerfectInfo[II] = Gain[II] :+: Lose[II] :+: CNil
  type ImperfectInfo[II] = ImperfectInfoExchange[II] :+: PerfectInfo[II]
}

trait ResourceInventories[II, Transaction, T[_]] {
  def players(t: T[II]): Seq[Int]

  def numCards(t: T[II], player: Int): Int

  def canSpend(t: T[II], player: Int, resSet: InventorySet[II, Int]): Boolean

  def update(t: T[II], transactions: List[Transaction]): T[II]

}

object ResourceInventories {

  type PublicInventories[_] = Map[Int, Int]
  type PrivateInventories[II] = Map[Int, InventorySet[II, Int]]

  implicit class ResourceInventoriesOp[II, Transactions, T[_]](inv: T[II])(implicit ev: ResourceInventories[II, Transactions, T]) {

    def numCards(player: Int): Int = ev.numCards(inv, player)

    def canSpend(player: Int, resSet: InventorySet[II, Int]): Boolean = ev.canSpend(inv, player, resSet)

    def update(transactions: List[Transactions]): T[II] = ev.update(inv, transactions)

    def update(transactions: Transactions*): T[II] = update(transactions.toList)

    def toPublic: PublicInventories[II] = ev.players(inv).map(p => p -> numCards(p)).toMap
  }

  object PublicTransactionPoly extends Poly2 {
    implicit def gain[II]: Case.Aux[Map[Int, Int], Gain[II], Map[Int, Int]] =
      at { case (m, g) => m.get(g.player).fold(m)(i => m + (g.player -> (i + g.set.getTotal))) }

    implicit def lose[II]: Case.Aux[Map[Int, Int], Lose[II], Map[Int, Int]] =
      at { case (m, g) => m.get(g.player).fold(m)(i => m + (g.player -> (i - g.set.getTotal))) }

    implicit def exchange[II]: Case.Aux[Map[Int, Int], ImperfectInfoExchange[II], Map[Int, Int]] =
      at { case (m, g) =>
        m.get(g.to).zip(m.get(g.from)).fold(m) { case (t, f) =>
          m + (g.to -> (t + 1)) + (g.from -> (f - 1))
        }
      }
  }

  object PerfectInfoTransactionPoly extends Poly2 {
    implicit def gain[II]: Case.Aux[Map[Int, InventorySet[II, Int]], Gain[II], Map[Int, InventorySet[II, Int]]] =
      at { case (m, g) => m.get(g.player).fold(m)(i => m + (g.player -> i.add(g.set))) }

    implicit def lose[II]: Case.Aux[Map[Int, InventorySet[II, Int]], Lose[II], Map[Int, InventorySet[II, Int]]] =
      at { case (m, g) => m.get(g.player).fold(m)(i => m + (g.player -> i.subtract(g.set))) }
  }


  implicit def publicResourceInventories[II, RT <: Coproduct](implicit transEmbed: Embedder[ImperfectInfo[II], RT]): ResourceInventories[II, RT, PublicInventories] = {
    new ResourceInventories[II, RT, PublicInventories] {

      override def numCards(t: PublicInventories[II], player: Int): Int =
        t.getOrElse(player, 0)

      override def canSpend(t: PublicInventories[II], player: Int, resSet: InventorySet[II, Int]): Boolean =
        numCards(t, player) > resSet.getTotal

      override def update(m: PublicInventories[II], transactions: List[RT]): PublicInventories[II] = {
        transactions.foldLeft(m) { case (m, transaction) =>
          transEmbed.embed(transaction).foldLeft(m)(PublicTransactionPoly)
        }
      }

      override def players(t: PublicInventories[II]): Seq[Int] = t.keys.toSeq
    }
  }

  implicit def privateResourceInventories[II]: ResourceInventories[II, PerfectInfo[II], PrivateInventories] = {
    new ResourceInventories[II, PerfectInfo[II], PrivateInventories] {

      private def playerInvSet(t: PrivateInventories[II], player: Int) = t.getOrElse(player, InventorySet.empty[II, Int])

      override def numCards(t: PrivateInventories[II], player: Int): Int = playerInvSet(t, player).getTotal

      override def canSpend(t: PrivateInventories[II], player: Int, resSet: InventorySet[II, Int]): Boolean = {
        playerInvSet(t, player).contains(resSet)
      }

      override def update(m: PrivateInventories[II], transactions: List[PerfectInfo[II]]): PrivateInventories[II] = {
        transactions.foldLeft(m) { case (m, transaction) =>
          transaction.foldLeft(m)(PerfectInfoTransactionPoly)
        }
      }

      override def players(t: PrivateInventories[II]): Seq[Int] = t.keys.toSeq
    }
  }

//  implicit def initPublicInv[Res](implicit ids: PlayerIds) = new StateInitializer[PublicInventories[Res]] {
//    override def apply(): PublicInventories[Res] = ids.players.map(_ -> 0).toMap
//  }
//
//  implicit def initPrivateInv[Res](implicit ids: PlayerIds) = new StateInitializer[PrivateInventories[Res]] {
//    override def apply(): PrivateInventories[Res] = ids.players.map(_ -> InventorySet.empty[Res, Int]).toMap
//  }
}