package soc.core

import soc.board.{BoardConfiguration, BoardRules}
import soc.inventory.developmentCard.DevelopmentCardSet
import soc.inventory.developmentCard.DevelopmentCardSet.DevelopmentCardSet
import soc.inventory.resources.ResourceSet
import soc.inventory.resources.ResourceSet.Resources

case class GameRules[B <: BoardConfiguration](
  pointsToWin: Int,
  initBank: Resources,
  initDevCardAmounts: DevelopmentCardSet[Int],
  numSettlements: Int,
  numCities: Int,
  numRoads: Int,
  discardLimit: Int,
  longestRoad: Int,
  largestArmy: Int,
  boardRules: BoardRules[B]
) extends BoardRules[B]

object GameRules {

    val RESOURCE_COUNT = 19
    val KNIGHT_COUNT = 14
    val MONOPOLY_COUNT = 2
    val YOP_COUNT = 2
    val ROAD_BUILDER_COUNT = 2
    val VP_COUNT = 5

    val POINTS_TO_WIN = 10
    val INITIAL_BANK = ResourceSet.fullBank
    val INITIAL_DEV_AMOUNTS = DevelopmentCardSet(KNIGHT_COUNT, VP_COUNT, ROAD_BUILDER_COUNT, MONOPOLY_COUNT, YOP_COUNT)
    val SETTLEMENTS_COUNT = 5
    val CITIES_COUNT = 4
    val ROADS_COUNT = 15

    val ROBBER_ROLL = 7

    val DISCARD_RESOURCE_LIMIT = 7

    val LONGEST_ROAD = 5
    val LARGEST_ARMY = 3

    def default[B <: BoardConfiguration](implicit boardRules: BoardRules[B]): GameRules[B] = GameRules(
      GameRules.POINTS_TO_WIN,
      GameRules.INITIAL_BANK,
      GameRules.INITIAL_DEV_AMOUNTS,
      GameRules.SETTLEMENTS_COUNT,
      GameRules.CITIES_COUNT,
      GameRules.ROADS_COUNT,
      GameRules.DISCARD_RESOURCE_LIMIT,
      GameRules.LONGEST_ROAD,
      GameRules.LARGEST_ARMY,
      boardRules
    )
}
