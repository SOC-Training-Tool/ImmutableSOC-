package soc

import shapeless.{:+:, CNil, Coproduct}

package object base {

  case object Knight

  case object Point

  case object RoadBuilder

  case object Monopoly

  case object YearOfPlenty

  type DevelopmentCard = Knight.type :+: Point.type :+: RoadBuilder.type :+: Monopoly.type :+: YearOfPlenty.type :+: CNil

  object DevelopmentCards {
    val KNIGHT: DevelopmentCard = Coproduct[DevelopmentCard](Knight)
    val POINT: DevelopmentCard = Coproduct[DevelopmentCard](Point)
    val ROAD_BUILDER: DevelopmentCard = Coproduct[DevelopmentCard](RoadBuilder)
    val MONOPOLY: DevelopmentCard = Coproduct[DevelopmentCard](Monopoly)
    val YEAR_OF_PLENTY: DevelopmentCard = Coproduct[DevelopmentCard](YearOfPlenty)


  }

}
