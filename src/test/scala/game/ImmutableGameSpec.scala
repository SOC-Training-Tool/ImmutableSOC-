package game

import org.scalatest.{FunSpec, Matchers}
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

  val stringExtension =
    new ActionExtension[String, Double :: HNil] {
      override def apply(move: String, pre: STATE, post: STATE): STATE =
        post.updatedElem(move.toDouble)
    }

  type GM = String :+: Int :+: CNil

  describe("ImmutableGame") {

    it("should update the state") {
      val game = ImmutableGame.builder
        .addAction(IntAction)
        .addAction(StringAction)
        .build

      val state = "" :: 0 :: HNil

      val result1 = game.apply(Coproduct[GM](1), state)

      result1.select[Int] shouldBe 1
      result1.select[String] shouldBe ""
    }

    it("should update the state with an extended action") {


      val game = ImmutableGame.builder
        .addAction(IntAction)
        .addAction(StringAction.extend(stringExtension))
        .build

      val state = ImmutableGame.initialize[Double :: String :: Int :: HNil]

      val result2 = game.apply(Coproduct[GM]("0.1"), state)

      result2.select[Int] shouldBe 0
      result2.select[String] shouldBe "0.1"
      result2.select[Double] shouldBe 0.1
    }
  }
}
