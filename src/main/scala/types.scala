package types

import types.primitive.GoodCategorical

import scalaz._
import Scalaz._

/**
  * primitive types
  */
object primitive {

  sealed abstract class Primitive[V,S <: Primitive[V,S]](v: V)
  (
    implicit val zero: ()=>V,
    implicit val canon: ()=>V=>V
  ) {
    // class representative
    val repr: V = canon()(v)

    // equality
    // XXX someone should check this
    override def hashCode = 41*this.getClass.hashCode + repr.hashCode
    override def equals(o: Any) = o match {
      case that: S => (this.getClass == that.getClass) && (this.repr == that.repr)
      case _ => false
    }
  }

  sealed abstract class PrimitiveCompanion[V,S <: Primitive[V,S]]
  (
    implicit val zero: ()=>V,
    implicit val canon: ()=>V=>V
  ) {
    // unit
    val nil : S = apply(zero())

    // constructor and extractor methods
    def apply(v: V): S
    def unapply(s: S) = some(s.repr)

    implicit val eqV: Equal[V] = Equal.equal((v1: V, v2: V) => canon()(v1) == canon()(v2))
    implicit val eq: Equal[S] = Equal.equal((s1: S, s2: S) => s1.repr === s2.repr)
    implicit def toV(s: S) = s.repr
  }

  // scalar with transformations defined in implicits (see implicits package)
  import implicits.defaults.Sca

  class GoodScalar private(v: Sca) extends Primitive[Sca,GoodScalar](v)
  object GoodScalar extends PrimitiveCompanion[Sca,GoodScalar] {
    override def apply(v: Sca) = new GoodScalar(v)
  }

  // categorical with transformations defined in implicits (see implicits package)
  import implicits.defaults.Cat

  class GoodCategorical private(v: Cat) extends Primitive[Cat,GoodCategorical](v)
  object GoodCategorical extends PrimitiveCompanion[Cat,GoodCategorical] {
    override def apply(v: Cat): GoodCategorical = new GoodCategorical(v)
  }

}


/**
  * XXX HERE BE DRAGONS DON'T LOOK BELOW THIS LINE XXX
  */

/**
  * statistical types that accumulate primitive types
  */
object statistical {
  import primitive._

  type Count = Long
  type Categorical = GoodCategorical
  type Scalar = GoodScalar

  sealed trait Cumulative[S <: Cumulative[S]] {
    // unit
    def nil: S

    // class representative
    val repr: S

    // equivalence relation
    def equiv(other: S): Boolean = this.repr == other.repr

    // addition
    def add(other: S): S
  }
  object Cumulative {
    def eqFactory[S <: Cumulative[S]]: Equal[S] = Equal.equal((s1: S, s2: S) => s1.equiv(s2))
    def monoidFactory[S <: Cumulative[S]](nil: S): Monoid[S] = new Monoid[S] {
      override def zero: S = nil

      override def append(f1: S, f2: => S): S = f1.add(f2)
    }
  }

  // cumulative (observations that can be combined to make statistics)
  abstract class QuantitativeAccumulator[Self<:QuantitativeAccumulator[Self]](implicit val eq: Equal[Self], implicit val mon: Monoid[Self])

  trait QuantitativeAccumulatorCompanion[Q] {
    // identity
    def identity: Q

    // class representative (should be idempotent)
    def repr(q: Q): Q

    // class addition
    def add(q1: Q, q2: =>Q): Q

    implicit val eq = Equal.equal(repr(_) == repr(_))
    implicit val mon = new Monoid[Q] {
      override def zero: Q = repr(identity)
      override def append(f1: Q, f2: => Q): Q = add(repr(f1),repr(f2))
    }
  }

  // numeric that keeps track of second-order statistics
  case class NumericQuadraticAccumulator(count: CountValue, sum: ScalarValue, sum2: ScalarValue) extends QuantitativeAccumulator[NumericQuadraticAccumulator]

  object NumericQuadraticAccumulator extends QuantitativeAccumulatorCompanion[NumericQuadraticAccumulator] {
    override def identity: NumericQuadraticAccumulator = NumericQuadraticAccumulator(
      Monoid[CountValue].zero,
      Monoid[ScalarValue].zero,
      Monoid[ScalarValue].zero
    )

    override def repr(q: NumericQuadraticAccumulator): NumericQuadraticAccumulator = q match {
      case
    }

    override def add(q1: NumericQuadraticAccumulator, q2: => NumericQuadraticAccumulator): NumericQuadraticAccumulator = ???
  }

  implicit val numericQuadraticSummableClass = new SummableClass[NumericQuadraticAccumulator] {
    override def repr: NumericQuadraticAccumulator =
    override def zero: NumericQuadraticAccumulator = NumericQuadraticAccumulator(
      Monoid[CountValue].zero,
      Monoid[ScalarValue].zero,
      Monoid[ScalarValue].zero
    )

    override def append(f1: NumericQuadraticAccumulator, f2: => NumericQuadraticAccumulator): NumericQuadraticAccumulator =
      NumericQuadraticAccumulator(f1.count |+| f2.count, f1.sum |+| f2.sum, f1.sum2 |+| f2.sum2)

    override def equal(a1: NumericQuadraticAccumulator, a2: NumericQuadraticAccumulator): Boolean =
      ( (f1.count === Monoid[ScalarValue].zero) && (f2.count === Monoid[ScalarValue].zero) ) ||
        ( (f1.sum ===
          (f1.count === f2.count) && (f1.sum === f2.sum) && (f1.sum2 === f2.sum2)
  }

  // categorical
  case class CategoricalAccumulator(freqMap: Map[CategoricalValue,CountValue]) extends QuantitativeAccumulator[CategoricalAccumulator]

}
