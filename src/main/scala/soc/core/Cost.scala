package soc.core

import soc.inventory.{CatanSet, InventoryItem}

trait Cost[II <: InventoryItem, A] {
  def getCost: CatanSet[II, Int]
}

object Cost {
  def apply[II <: InventoryItem, A](set: CatanSet[II, Int]): Cost[II, A] = new Cost[II, A] {
    override def getCost: CatanSet[II, Int] = set
  }
}