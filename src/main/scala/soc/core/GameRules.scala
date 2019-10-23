package soc.core

import soc.inventory.developmentCard.DevelopmentCardSet
import soc.inventory.developmentCard.DevelopmentCardSet.PlayedInventory
import soc.inventory.resources.CatanResourceSet
import soc.inventory.resources.CatanResourceSet.Resources

case class GameRules(
  pointsToWin: Int,
  initBank: Resources,
  initDevCardAmounts: PlayedInventory,
  numSettlements: Int,
  numCities: Int,
  numRoads: Int,
  discardLimit: Int
)

object GameRules {
  val RESOURCE_COUNT = 19
  val KNIGHT_COUNT = 14
  val MONOPOLY_COUNT = 2
  val YOP_COUNT = 2
  val ROAD_BUILDER_COUNT = 2
  val VP_COUNT = 5

  val POINTS_TO_WIN = 10
  val INITIAL_BANK = CatanResourceSet(RESOURCE_COUNT, RESOURCE_COUNT, RESOURCE_COUNT, RESOURCE_COUNT, RESOURCE_COUNT)
  val INITIAL_DEV_AMOUNTS = DevelopmentCardSet(KNIGHT_COUNT, VP_COUNT, ROAD_BUILDER_COUNT, MONOPOLY_COUNT, YOP_COUNT)
  val SETTLEMENTS_COUNT = 5
  val CITIES_COUNT = 4
  val ROADS_COUNT = 15

  val DISCARD_RESOURCE_LIMIT = 7

  def default = GameRules(
    GameRules.POINTS_TO_WIN,
    GameRules.INITIAL_BANK,
    GameRules.INITIAL_DEV_AMOUNTS,
    GameRules.SETTLEMENTS_COUNT,
    GameRules.CITIES_COUNT,
    GameRules.ROADS_COUNT,
    GameRules.DISCARD_RESOURCE_LIMIT)


}
