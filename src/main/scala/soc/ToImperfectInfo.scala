package soc

import game.PerfectInfoMoveResult
import shapeless.{Coproduct, Poly1}
import shapeless.ops.coproduct

trait ToImperfectInfo[Imperfect, Perfect] extends shapeless.DepFn1[Perfect] {
  override type Out = Imperfect
}

object ToImperfectInfo {

  private object PerfectToImperfectMovesPoly extends Poly1 {
    implicit def toImperfect[P <: PerfectInfoMoveResult]: PerfectToImperfectMovesPoly.Case.Aux[P, P#ImperfectInfoMoveResult] = at[P](_.getPerspectiveResults(Seq(-1)).head._2)

    implicit def default[Perfect, Imperfect](implicit toImperfect: ToImperfectInfo[Imperfect, Perfect]): Case.Aux[Perfect, Imperfect] = at[Perfect](toImperfect.apply)

  }

  implicit def movesCoproduct[ImperfectInfoMoves <: Coproduct, PerfectInfoMoves <: Coproduct, MapOut <: Coproduct]
  (implicit mapper: coproduct.Mapper.Aux[PerfectToImperfectMovesPoly.type, PerfectInfoMoves, MapOut],
   basis: coproduct.Basis[ImperfectInfoMoves, MapOut]
  ): ToImperfectInfo[ImperfectInfoMoves, PerfectInfoMoves] =
    new ToImperfectInfo[ImperfectInfoMoves, PerfectInfoMoves] {
      override def apply(perfectInfoMove: PerfectInfoMoves): ImperfectInfoMoves = {
        val outCoproduct: MapOut = perfectInfoMove.map(PerfectToImperfectMovesPoly)
        outCoproduct.embed[ImperfectInfoMoves]
      }
    }
}
