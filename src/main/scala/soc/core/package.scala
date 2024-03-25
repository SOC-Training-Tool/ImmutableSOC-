package soc


import game.{InventorySet, StateInitializer}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil}
import soc.core.ResourceInventories
import soc.core.Resources._
import util.DependsOn
package object core {

  case object Wood

  case object Brick

  case object Sheep

  case object Wheat

  case object Ore

  type Resource = Wood.type :+: Brick.type :+: Sheep.type :+: Wheat.type :+: Ore.type :+: CNil

  object Resources {
    val WOOD: Resource = Coproduct[Resource](Wood)
    val BRICK: Resource = Coproduct[Resource](Brick)
    val SHEEP: Resource = Coproduct[Resource](Sheep)
    val WHEAT: Resource = Coproduct[Resource](Wheat)
    val ORE: Resource = Coproduct[Resource](Ore)

    val all: Seq[Resource] = WOOD :: BRICK :: SHEEP :: WHEAT :: ORE :: Nil

  }

  object ResourceSet {
    type ResourceSet[T] = InventorySet[Resource, T]
    type Resources = ResourceSet[Int]

    def apply[T: Numeric](br: T = 0, or: T = 0, sh: T = 0, wh: T = 0, wo: T = 0): ResourceSet[T] = ResourceSet[T](Map[Resource, T](BRICK -> br, WOOD -> wo, ORE -> or, SHEEP -> sh, WHEAT -> wh))

    def apply[T: Numeric](resMap: Map[Resource, T]): ResourceSet[T] = InventorySet.fromMap(resMap)

    def apply(resources: Resource*): Resources = InventorySet.fromList(resources.toSeq)

    def empty[T: Numeric]: ResourceSet[T] = InventorySet.empty[Resource, T]
  }


  case object Settlement

  case object City

  case object Road

  trait VertexBuildingValue[A] {
    def apply: Int
  }

  type VertexBuilding = Settlement.type :+: City.type :+: CNil

  type EdgeBuilding = Road.type :+: CNil

  object VertexBuilding {

    val SETTLEMENT: VertexBuilding = Coproduct[VertexBuilding](Settlement)
    val CITY: VertexBuilding = Coproduct[VertexBuilding](City)

    implicit val settlementValue: VertexBuildingValue[Settlement.type] = new VertexBuildingValue[Settlement.type] {
      override def apply: Int = 1
    }

    implicit val cityValue: VertexBuildingValue[City.type] = new VertexBuildingValue[City.type] {
      override def apply: Int = 2
    }
  }

  object EdgeBuilding {
    val ROAD = Coproduct[EdgeBuilding](Road)
  }


}
