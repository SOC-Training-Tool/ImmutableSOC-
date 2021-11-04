package util

import shapeless.{::, DepFn0, HList, HNil}

object hlist {

  trait TypeWrapper[T, W[_ <: T], A <: T] extends DepFn0 {
    override type Out = W[A]
  }

  trait HListWrapper[T, W[_ <: T], L <: HList] extends DepFn0 {
    type Out <: HList
  }

  object HListWrapper {

    type Aux[T, W[_ <: T], L <: HList, Out0] = HListWrapper[T, W, L] { type Out = Out0}

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

}