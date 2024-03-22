package game

import org.scalatest.{FunSpec, Matchers}
import shapeless.ops.{coproduct, hlist}
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil}
import util.DependsOn

class ImmutableGameSpec extends FunSpec with Matchers {


  object IntAction extends GameAction[Int, Int :: HNil] {
    override def applyMove[GAME_STATE <: HList](move: Int, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, Int :: HNil]) = {
      val i = dep.get[Int](state)
      dep.update(i + move, state)
    }
  }

  object StringAction extends GameAction[String, String :: HNil] {
    override def applyMove[GAME_STATE <: HList](move: String, state: GAME_STATE)(implicit dep: DependsOn[GAME_STATE, String :: HNil]) = {
      val s = dep.get[String](state)
      dep.update(s"$s$move", state)
    }
  }

  type GM = String :+: Int :+: CNil
  type S = String :: Int :: HNil

  describe("ImmutableGame") {

    it("should update the state") {

//      val stringAction2 = StringAction.extend[Double :: HNil].apply { case (move, state) =>
//        state.updateWith[Double, Double, Double :: HNil](_ => move.toDouble)
//      }

      val f = coproduct.ExtendLeftBy[String :+: CNil, Int :+: CNil]
      val g = hlist.Prepend[String :: HNil, Int :: HNil]

      val game = ImmutableGame.builder
        .addAction(IntAction)
        .addAction(StringAction)
        .build.apply()

      val state: S = "":: 0 :: HNil

      val result1 = game.apply(Coproduct[GM](1), state)

      result1.select[Int] shouldBe 1
      result1.select[String] shouldBe ""

//      val result2 = game.applyMove().apply().apply(Coproduct[GM]("0.1"), result1)
//
//      result2.select[Int] shouldBe 1
//      result2.select[String] shouldBe "0.1"
//      result2.select[Double] shouldBe 0.1
    }
  }

}
