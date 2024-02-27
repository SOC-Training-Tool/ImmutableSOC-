package soc.core

import shapeless.Coproduct
import soc.inventory.{CatanSet, InventoryItem}

trait Cost[II <: Coproduct, A] {
  def getCost: CatanSet[II, Int]
}

object Cost {
  def apply[II <: Coproduct, A](set: CatanSet[II, Int]): Cost[II, A] = new Cost[II, A] {
    override def getCost: CatanSet[II, Int] = set
  }
}