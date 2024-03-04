package soc.inventory

import game.InventorySet
import shapeless.{::, Coproduct, HNil}
import soc.core.{Gain, Lose, SOCTransactions, Steal}

trait ResourceInventories[II <: Coproduct, T] {

  def players(t: T): Seq[Int]

  def numCards(t: T, player: Int): Int

  def canSpend(t: T, player: Int, resSet: InventorySet[II, Int]): Boolean

  def update(t: T, transactions: List[SOCTransactions[II]]): T

}

object ResourceInventories {

  type PublicInventories[II <: Coproduct] = Map[Int, Int] :: HNil
  type PrivateInventories[II <: Coproduct] = Map[Int, InventorySet[II, Int]] :: HNil

  implicit class ResourceInventoriesOp[II <: Coproduct, T](inv: T)(implicit ev: ResourceInventories[II, T]) {

    def numCards(player: Int): Int = ev.numCards(inv, player)

    def canSpend(player: Int, resSet: InventorySet[II, Int]): Boolean = ev.canSpend(inv, player, resSet)

    def update(transactions: List[SOCTransactions[II]]): T = ev.update(inv, transactions)

    def toPublic: PublicInventories[II] = ev.players(inv).map(p => p -> numCards(p)).toMap :: HNil
  }


  implicit def publicResourceInventories[II <: Coproduct]: ResourceInventories[II, PublicInventories[II]] = new ResourceInventories[II, PublicInventories[II]] {
    override def numCards(t: PublicInventories[II], player: Int): Int =
      t.select[Map[Int, Int]].getOrElse(player, 0)

    override def canSpend(t: PublicInventories[II], player: Int, resSet: InventorySet[II, Int]): Boolean =
      numCards(t, player) > resSet.getTotal

    override def update(t: PublicInventories[II], transactions: List[SOCTransactions[II]]): PublicInventories[II] = {
      t.updateWith[Map[Int, Int], Map[Int, Int], Map[Int, Int] :: HNil](m => transactions.foldLeft(m) {
        case (map, Gain(player, set)) =>
          map + (player -> (map.getOrElse(player, 0) + set.getTotal))
        case (map, Lose(player, set)) =>
          map + (player -> (map.getOrElse(player, 0) - set.getTotal)) // negative number?
        case (map, Steal(robber, victim, Some(set))) =>
          map +
            (robber -> (map.getOrElse(robber, 0) + set.getTotal)) +
            (victim -> (map.getOrElse(victim, 0) - set.getTotal))
        case (map, _) => map
      })
    }

    override def players(t: PublicInventories[II]): Seq[Int] = t.select[Map[Int, Int]].keys.toSeq
  }

  implicit def privateResourceInventories[II <: Coproduct]: ResourceInventories[II, PrivateInventories[II]] = new ResourceInventories[II, PrivateInventories[II]] {

    private def playerInvSet(t: PrivateInventories[II], player: Int) = t.select[Map[Int, InventorySet[II, Int]]].getOrElse(player, InventorySet.empty)

    override def numCards(t: PrivateInventories[II], player: Int): Int = playerInvSet(t, player).getTotal

    override def canSpend(t: PrivateInventories[II], player: Int, resSet: InventorySet[II, Int]): Boolean = {
       playerInvSet(t, player).contains(resSet)
    }

    override def update(t: PrivateInventories[II], transactions: List[SOCTransactions[II]]): PrivateInventories[II] = {
      t.updateWith[Map[Int, InventorySet[II, Int]], Map[Int, InventorySet[II, Int]], Map[Int, InventorySet[II, Int]] :: HNil](m => transactions.foldLeft(m) {
        case (map, Gain(player, set)) =>
          map + (player -> map.getOrElse(player, InventorySet.empty).add(set))
        case (map, Lose(player, set)) =>
          map + (player -> map.getOrElse(player, InventorySet.empty).subtract(set)) // negative number?
        case (map, Steal(robber, victim, Some(set))) =>
          map +
            (robber -> map.getOrElse(robber, InventorySet.empty).add(set)) +
            (victim -> map.getOrElse(victim, InventorySet.empty).subtract(set))
        case (map, _) => map
      })
    }

    override def players(t: PrivateInventories[II]): Seq[Int] = t.select[Map[Int, InventorySet[II, Int]]].keys.toSeq
  }
}