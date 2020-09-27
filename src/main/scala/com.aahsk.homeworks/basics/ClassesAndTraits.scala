package com.aahsk.homeworks.basics

// Homework
//
// Add additional 2D shapes such as triangle and square.
//
// In addition to the 2D shapes classes, add also 3D shapes classes
// (origin, point, sphere, cube, cuboid, 3D triangle - you can add
// others if you think they are a good fit).
//
// Add method `area` to 2D shapes.
//
// Add methods `surfaceArea` and `volume` to 3D shapes.
//
// If some of the implementation involves advanced math, it is OK
// to skip it (leave unimplemented), the primary intent of this
// exercise is modelling using case classes and traits, and not math.

object Traits {
  import Manipulations._

  sealed trait Dimensions[D <: Dimensions[D]]

  sealed trait Shape[D <: Dimensions[D]] extends Located[D] with Bounded[D] with Moveable[D]

  sealed trait Located[D <: Dimensions[D]] {
    val position: Dimensions[D]
  }

  sealed trait Bounded[D <: Dimensions[D]] {
    def min: Dimensions[D]
    def max: Dimensions[D]
  }

  sealed trait LocatedAndSymmetric[D <: Dimensions[D]] extends Located[D] with Bounded[D] {
    def boundSize: Dimensions[D]
    def min: Dimensions[D] = position subtract boundSize
    def max: Dimensions[D] = position add boundSize
  }

  sealed trait Moveable[D <: Dimensions[D]] {
    def move(delta: Dimensions[D]): Shape[D]
  }
}

object Objects {
  import Traits._;
  import Manipulations._;

  final case class D1(x: Double = 0)                               extends Dimensions[D1]
  final case class D2(x: Double = 0, y: Double = 0)                extends Dimensions[D2]
  final case class D3(x: Double = 0, y: Double = 0, z: Double = 0) extends Dimensions[D3]

  final case class Point[D <: Dimensions[D]](position: Dimensions[D]) extends Shape[D] {
    def min: Dimensions[D]                   = position
    def max: Dimensions[D]                   = position
    def move(delta: Dimensions[D]): Point[D] = Point(addDimensions(position, delta))
  }

  object OriginD1 extends Located[D1] {
    val position: Dimensions[D1] = D1()
  }

  object OriginD2 extends Located[D2] {
    val position: Dimensions[D2] = D2()
  }

  object OriginD3 extends Located[D3] {
    val position: Dimensions[D3] = D3()
  }

  final case class Circle(center: Dimensions[D2], radius: Double)
      extends Shape[D2]
      with LocatedAndSymmetric[D2] {
    val position: Dimensions[D2]            = center
    val boundSize: Dimensions[D2]           = D2(radius, radius)
    def move(delta: Dimensions[D2]): Circle = Circle(center add delta, radius)
  }

  final case class Rectangle(position: Dimensions[D2], boundSize: Dimensions[D2])
      extends Shape[D2]
      with LocatedAndSymmetric[D2] {
    def move(delta: Dimensions[D2]): Rectangle = Rectangle(position add delta, boundSize)
  }

  final case class Cuboid(position: Dimensions[D3], boundSize: Dimensions[D3])
      extends Shape[D3]
      with LocatedAndSymmetric[D3] {
    def move(delta: Dimensions[D3]): Cuboid = Cuboid(position add delta, boundSize)
  }

  final case class Cube(position: Dimensions[D3], size: Double)
      extends Shape[D3]
      with LocatedAndSymmetric[D3] {
    val boundSize: Dimensions[D3]         = D3(size, size, size)
    def move(delta: Dimensions[D3]): Cube = Cube(position add delta, size)
  }
}

object Manipulations {
  import Traits._;
  import Objects._;

  def addDimensions[D <: Dimensions[D]](d1: Dimensions[D], d2: Dimensions[D]): Dimensions[D] =
    (d1, d2) match {
      case (D1(x1), D1(x2))         => D1(x1 + x2).asInstanceOf[Dimensions[D]]
      case (D2(x1, y1), D2(x2, y2)) => D2(x1 + x2, y1 + y2).asInstanceOf[Dimensions[D]]
      case (D3(x1, y1, z1), D3(x2, y2, z2)) =>
        D3(x1 + x2, y1 + y2, z1 + z2).asInstanceOf[Dimensions[D]]
    }

  def negateDimensions[D <: Dimensions[D]](d: Dimensions[D]): Dimensions[D] =
    d match {
      case (D1(x))       => D1(-x).asInstanceOf[Dimensions[D]]
      case (D2(x, y))    => D2(-x, -y).asInstanceOf[Dimensions[D]]
      case (D3(x, y, z)) => D3(-x, -y, -z).asInstanceOf[Dimensions[D]]
    }

  implicit class DimensionOps[D <: Dimensions[D]](value: Dimensions[D]) {
    def negate(): Dimensions[D]                   = negateDimensions(value)
    def add(value2: Dimensions[D]): Dimensions[D] = addDimensions(value, value2)
    def subtract(value2: Dimensions[D]): Dimensions[D] =
      addDimensions(value, negateDimensions(value2))
  }

//  def minimumBound[D <: Dimensions[D]](objects: Set[Bounded[D]]): Bounded[D] = {
//    objects
////    new Bounded[Dimensions[D]] {
////      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering
////
////    }
//  }

//  implicit class BoundedOps[A, D <: Set[Bounded[D]]](var objects: A) {
//    def minimumBoundingRectangle(): Bounded[D] = Manipulations.minimumBound[D](objects)
//  }
}
