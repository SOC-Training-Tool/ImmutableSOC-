package soc.base.actions

import game._
import shapeless.{::, Coproduct, HNil}
import soc.base.state.RobberLocation
import soc.base.state.ops.RobberStateOps
import soc.base.{PerfectInfoRobberMoveResult, RobberMoveResult}
import soc.inventory.ResourceInventories
import soc.inventory.ResourceInventories.ResourceInventoriesOp
import soc.inventory.Transactions._
import util.DependsOn

object MoveRobberAndStealAction {

  def apply[II, INV[_]]
  (implicit inv: ResourceInventories[II, ImperfectInfo[II], INV]): GameAction[RobberMoveResult[II], RobberLocation :: INV[II] :: HNil] = {
    GameAction[RobberMoveResult[II], RobberLocation :: INV[II] :: HNil] { case (move, state) =>
      val dep = DependsOn.single[RobberLocation :: INV[II] :: HNil]
      implicit val robberDep = dep.innerDependency[RobberLocation :: HNil]
      val result = state.updateRobberHexId(move.robberHexId)
      move.steal.fold(result) { stl =>
        dep.updateWith[INV[II]](result)(_.update(
          Coproduct[ImperfectInfo[II]](ImperfectInfoExchange(stl.victim, move.player, stl.resource))))
      }
    }
  }

  def perfectInfo[II, INV[_]](implicit inv: ResourceInventories[II, PerfectInfo[II], INV]) = {
    GameAction[PerfectInfoRobberMoveResult[II], RobberLocation :: INV[II] :: HNil] { case (move, state) =>
      val dep = DependsOn.single[RobberLocation :: INV[II] :: HNil]
      implicit val robberDep = dep.innerDependency[RobberLocation :: HNil]
      val result = state.updateRobberHexId(move.robberHexId)
      move.steal.fold(result) { stl =>
        val set = InventorySet.fromList(Seq(stl.resource))
        dep.updateWith[INV[II]](result)(_.update(
          Coproduct[PerfectInfo[II]](Gain(move.player, set)),
          Coproduct[PerfectInfo[II]](Lose(stl.victim, set))))
      }
    }
  }
}
