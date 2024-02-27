package soc.inventory

import shapeless.{:+:, CNil, Coproduct}
import soc.inventory.resources.ResourceSet

sealed trait InventoryItem

sealed trait Port {
  val value: Int
}

case object Wood
case object Sheep
case object Wheat
case object Brick
case object Ore

object Resource {

  type Resource = Wood.type :+: Sheep.type :+: Wheat.type :+: Brick.type :+: Ore.type :+: CNil

  val WOOD = Coproduct[Resource](Wood)
  val SHEEP = Coproduct[Resource](Sheep)
  val WHEAT = Coproduct[Resource](Wheat)
  val BRICK = Coproduct[Resource](Brick)
  val ORE = Coproduct[Resource](Ore)


  val list: List[Resource] = List(WOOD, SHEEP, WHEAT, BRICK, ORE)
}


case object Misc extends Port {
  val value = 0
}

sealed trait DevelopmentCard extends InventoryItem {
  val initAmount: Int
}

case object Knight extends DevelopmentCard {
  override val initAmount: Int = 14
}

case object CatanPoint extends DevelopmentCard {
  override val initAmount: Int = 5
}

case object RoadBuilder extends DevelopmentCard {
  override val initAmount: Int = 2
}

case object Monopoly extends DevelopmentCard {
  override val initAmount: Int = 2
}

case object YearOfPlenty extends DevelopmentCard {
  override val initAmount: Int = 2
}

sealed trait CatanBuilding extends InventoryItem {
  val playerId: Int
}

sealed trait VertexBuilding extends CatanBuilding

sealed trait EdgeBuilding extends CatanBuilding

case class Settlement(playerId: Int) extends VertexBuilding

case class City(playerId: Int) extends VertexBuilding

case class Road(playerId: Int) extends EdgeBuilding

object Settlement {
  val cost = ResourceSet(wo = 1, sh = 1, br = 1, wh = 1)
}

object City {
  val cost = ResourceSet(or = 3, wh = 2)
}

object Road {
  val cost = ResourceSet(wo = 1, br = 1)
}

object DevelopmentCard {
  val cost = ResourceSet(or = 1, sh = 1, wh = 1)
}