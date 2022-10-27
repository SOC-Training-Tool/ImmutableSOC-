package util

import shapeless.ops.hlist.{Selector, ToTraversable}
import shapeless.{::, DepFn0, DepFn1, HList, HNil}

object hlist {

  trait TypeWrapper[T, W[_ <: T], A <: T] extends DepFn0 {
    override type Out = W[A]
  }

  trait HListWrapper[T, W[_ <: T], L <: HList] extends DepFn0 {
    type Out <: HList
  }

  object HListWrapper {

    type Aux[T, W[_ <: T], L <: HList, Out0] = HListWrapper[T, W, L] {type Out = Out0}

    def apply[T, W[_ <: T], L <: HList](implicit hlw: HListWrapper[T, W, L]): Aux[T, W, L, hlw.Out] = hlw

    implicit def wrapList[F, W[_ <: F], H <: F, T <: HList](implicit wrapper: TypeWrapper[F, W, H], next: HListWrapper[F, W, T]): Aux[F, W, H :: T, W[H] :: next.Out] = new HListWrapper[F, W, H :: T] {
      override type Out = W[H] :: next.Out

      override def apply(): Out = wrapper.apply() :: next.apply()
    }

    implicit def hNil[T, W[_ <: T]]: Aux[T, W, HNil, HNil] = new HListWrapper[T, W, HNil] {
      override type Out = HNil

      override def apply(): Out = HNil
    }

  }

  trait WrappedSelectAll[F, W[_ <: F], WL <: HList, SL <: HList] extends DepFn1[WL] {
    type Out <: HList
    def traversable(t: WL): Seq[W[_ <: F]]
  }

  object WrappedSelectAll {

    type Aux[F, W[_ <: F], WL <: HList, SL <: HList, Out0 <: HList] = WrappedSelectAll[F, W, WL, SL] {type Out = Out0}

    implicit def wrappedSelectAll[F, W[_ <: F], WL <: HList, H <: F, T <: HList](implicit s: Selector[WL, W[H]], wsa: WrappedSelectAll[F, W, WL, T]): Aux[F, W, WL, H :: T, W[H] :: wsa.Out] = new WrappedSelectAll[F, W, WL, H :: T] {
      override type Out = W[H] :: wsa.Out

      override def apply(t: WL): Out = s.apply(t) :: wsa.apply(t)

      override def traversable(t: WL): Seq[W[_ <: F]] = s.apply(t) +: wsa.traversable(t)


    }

    implicit def hNil[F, W[_ <: F], WL <: HList]: Aux[F, W, WL, HNil, HNil] = new WrappedSelectAll[F, W, WL, HNil] {
      override type Out = HNil

      override def apply(t: WL): Out = HNil

      override def traversable(t: WL): Seq[W[_ <: F]] = Nil
    }

  }


}