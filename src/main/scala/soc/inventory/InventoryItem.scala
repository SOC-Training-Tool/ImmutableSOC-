package soc.inventory

import soc.inventory.resources.ResourceSet

sealed trait InventoryItem

sealed trait Port
sealed abstract class Resource(val res: Int, val name: String) extends InventoryItem

object Resource {
  val list: List[Resource with Port] = List(Wood, Sheep, Wheat, Brick, Ore)
}

case object Wood extends Resource(1, "Wood") with Port
case object Sheep extends Resource(2, "Sheep") with Port
case object Wheat extends Resource(3, "Wheat") with Port
case object Brick extends Resource(4, "Brick") with Port
case object Ore extends Resource(5, "Ore") with Port

case object Misc extends Port

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