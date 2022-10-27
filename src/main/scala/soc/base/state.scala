package soc.base

import shapeless.{::, HList, HNil}
import soc.board.Vertex
import soc.inventory.Settlement
import util.{DependsOn, MapWrapper}

case class SOCSettlementMap(m: Map[Vertex, Settlement]) extends MapWrapper[Vertex, Settlement]

object SettlementSOCState {

  implicit class SettlementSOCStateOps[STATE <: HList](state: STATE)(implicit dep: DependsOn[STATE, SOCSettlementMap :: HNil]) {

  }

}