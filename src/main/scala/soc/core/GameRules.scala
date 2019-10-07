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
  numRoads: Int
)

object GameRules {
  val NUM_EACH_RESOURCE = 19
  val NUM_KNIGHT = 14
  val NUM_MONOPOLY = 2
  val NUM_YOP = 2
  val NUM_ROAD_BUILDER = 2
  val NUM_VP = 5

  val POINTS_TO_WIN = 10
  val INITIAL_BANK = CatanResourceSet(NUM_EACH_RESOURCE, NUM_EACH_RESOURCE, NUM_EACH_RESOURCE, NUM_EACH_RESOURCE, NUM_EACH_RESOURCE)
  val INITIAL_DEV_AMOUNTS = DevelopmentCardSet(NUM_KNIGHT, NUM_VP, NUM_ROAD_BUILDER, NUM_MONOPOLY, NUM_YOP)
  val NUM_SETTLEMENTS = 5
  val NUM_CITIES = 4
  val NUM_ROADS = 15

  def default = GameRules(
    GameRules.POINTS_TO_WIN,
    GameRules.INITIAL_BANK,
    GameRules.INITIAL_DEV_AMOUNTS,
    GameRules.NUM_SETTLEMENTS,
    GameRules.NUM_CITIES,
    GameRules.NUM_ROADS)


}
