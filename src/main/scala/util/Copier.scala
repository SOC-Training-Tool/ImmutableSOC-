package util

import shapeless._
import shapeless.ops.hlist.Replacer
import syntax.std.tuple._

case class Self[A](self: A)(implicit copier: Copier[A]) {
  def copy: A = copier.copy(self)
  def updateField[F](field: F): A = copier.updateField(field, self)
}

trait Copier[A <: Product] {
  type HListType <: HList
  def copy(a: A): A = a
  def updateField[F](f: F, obj: A)(implicit labelledGeneric: LabelledGeneric[A] { type Repr = HListType}, r: Replacer[HListType, F, F] {type Out = HListType}): A =
    labelledGeneric.from(labelledGeneric.to(obj).replace(f))
}

object Copier {


  def generateCopier[A <: Product, H <: HList] = new Copier[A] {
    override type HListType = H
  }

  def apply[E, A <: Product]: Copier[A] = generateCopier[A, E :: HNil]

  case class Tester(a: Int)
  implicit val lg = LabelledGeneric[Tester]
  Copier.apply[Int, Tester].updateField(1, Tester(2))



  def apply[E1, E2, A](apply: (E1, E2) => A, unapply: A => (E1, E2)): Copier[A] = new Copier[A] {
    val lgen = Generic[E1 :: E2 :: HNil]
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {

      val a: HNil = lgen.to(unapply(obj).productElements)

      val (e1, e2) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2))
    }
  }

  def apply[E1, E2, E3, A](apply: (E1, E2, E3) => A, unapply: A => (E1, E2, E3)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3))
    }
  }
  def apply[E1, E2, E3, E4, A](apply: (E1, E2, E3, E4) => A, unapply: A => (E1, E2, E3, E4)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4))
    }
  }
  def apply[E1, E2, E3, E4, E5, A](apply: (E1, E2, E3, E4, E5) => A, unapply: A => (E1, E2, E3, E4, E5)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, A](apply: (E1, E2, E3, E4, E5, E6) => A, unapply: A => (E1, E2, E3, E4, E5, E6)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, A](apply: (E1, E2, E3, E4, E5, E6, E7) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9), fieldUpdater[E10](f, e10))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9), fieldUpdater[E10](f, e10), fieldUpdater[E11](f, e11))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9), fieldUpdater[E10](f, e10), fieldUpdater[E11](f, e11), fieldUpdater[E12](f, e12))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9), fieldUpdater[E10](f, e10), fieldUpdater[E11](f, e11), fieldUpdater[E12](f, e12), fieldUpdater[E13](f, e13))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9), fieldUpdater[E10](f, e10), fieldUpdater[E11](f, e11), fieldUpdater[E12](f, e12), fieldUpdater[E13](f, e13), fieldUpdater[E14](f, e14))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9), fieldUpdater[E10](f, e10), fieldUpdater[E11](f, e11), fieldUpdater[E12](f, e12), fieldUpdater[E13](f, e13), fieldUpdater[E14](f, e14), fieldUpdater[E15](f, e15))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9), fieldUpdater[E10](f, e10), fieldUpdater[E11](f, e11), fieldUpdater[E12](f, e12), fieldUpdater[E13](f, e13), fieldUpdater[E14](f, e14), fieldUpdater[E15](f, e15), fieldUpdater[E16](f, e16))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9), fieldUpdater[E10](f, e10), fieldUpdater[E11](f, e11), fieldUpdater[E12](f, e12), fieldUpdater[E13](f, e13), fieldUpdater[E14](f, e14), fieldUpdater[E15](f, e15), fieldUpdater[E16](f, e16), fieldUpdater[E17](f, e17))
    }
  }
  def apply[E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17, E18, A](apply: (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17, E18) => A, unapply: A => (E1, E2, E3, E4, E5, E6, E7, E8, E9, E10, E11, E12, E13, E14, E15, E16, E17, E18)): Copier[A] = new Copier[A] {
    override def copy(a: A): A = apply.tupled(unapply(a))
    override def updateField[F](f: F, obj: A): A = {
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18) = unapply(obj)
      apply(fieldUpdater[E1](f, e1), fieldUpdater[E2](f, e2), fieldUpdater[E3](f, e3), fieldUpdater[E4](f, e4), fieldUpdater[E5](f, e5), fieldUpdater[E6](f, e6), fieldUpdater[E7](f, e7), fieldUpdater[E8](f, e8), fieldUpdater[E9](f, e9), fieldUpdater[E10](f, e10), fieldUpdater[E11](f, e11), fieldUpdater[E12](f, e12), fieldUpdater[E13](f, e13), fieldUpdater[E14](f, e14), fieldUpdater[E15](f, e15), fieldUpdater[E16](f, e16), fieldUpdater[E17](f, e17), fieldUpdater[E18](f, e18))
    }
  }



}


