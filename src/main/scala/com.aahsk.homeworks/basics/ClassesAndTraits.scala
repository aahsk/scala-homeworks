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

  sealed trait Dimensional[D <: Dimensional[D]]

  sealed trait Shaped[D <: Dimensional[D]] extends Located[D] with Bounded[D] with Moveable[D]

  sealed trait Surfaced {
    def surfaceArea: Double = area
    def area: Double
  }

  sealed trait Volumed {
    def volume: Double
  }

  sealed trait Located[D <: Dimensional[D]] {
    val position: Dimensional[D]
  }

  sealed trait Bounded[D <: Dimensional[D]] {
    def min: Dimensional[D]
    def max: Dimensional[D]
  }

  sealed trait LocatedAndSymmetric[D <: Dimensional[D]] extends Located[D] with Bounded[D] {
    val boundSize: Dimensional[D]
    def min: Dimensional[D] = position subtract boundSize
    def max: Dimensional[D] = position add boundSize
  }

  sealed trait Moveable[D <: Dimensional[D]] {
    def move(delta: Dimensional[D]): Shaped[D]
  }
}

object Objects {
  import Traits._;
  import Manipulations._;

  final case class D1(x: Double = 0)                               extends Dimensional[D1]
  final case class D2(x: Double = 0, y: Double = 0)                extends Dimensional[D2]
  final case class D3(x: Double = 0, y: Double = 0, z: Double = 0) extends Dimensional[D3]

  object OriginD1 extends Located[D1] {
    val position: Dimensional[D1] = D1()
  }

  object OriginD2 extends Located[D2] {
    val position: Dimensional[D2] = D2()
  }

  object OriginD3 extends Located[D3] {
    val position: Dimensional[D3] = D3()
  }

  final case class Point[D <: Dimensional[D]](position: Dimensional[D]) extends Shaped[D] {
    def min: Dimensional[D]                   = position
    def max: Dimensional[D]                   = position
    def move(delta: Dimensional[D]): Point[D] = Point(addDimensions(position, delta))
  }

  final case class Circle(center: Dimensional[D2], radius: Double)
      extends Shaped[D2]
      with LocatedAndSymmetric[D2]
      with Surfaced {
    val position: Dimensional[D2]            = center
    val boundSize: Dimensional[D2]           = D2(radius, radius)
    def move(delta: Dimensional[D2]): Circle = Circle(center add delta, radius)
    def area: Double                         = Math.PI * Math.pow(radius, 2)
  }

  final case class Rectangle(position: Dimensional[D2], boundSize: Dimensional[D2])
      extends Shaped[D2]
      with LocatedAndSymmetric[D2]
      with Surfaced {
    def move(delta: Dimensional[D2]): Rectangle = Rectangle(position add delta, boundSize)
    def area: Double                            = boundSize.x
  }

  final case class Cuboid(position: Dimensional[D3], boundSize: Dimensional[D3])
      extends Shaped[D3]
      with LocatedAndSymmetric[D3]
      with Surfaced
      with Volumed {
    def move(delta: Dimensional[D3]): Cuboid = Cuboid(position add delta, boundSize)
    def area: Double                         = ???
    def volume: Double                       = ???
  }

  final case class Cube(position: Dimensional[D3], size: Double)
      extends Shaped[D3]
      with LocatedAndSymmetric[D3]
      with Surfaced
      with Volumed {
    val boundSize: Dimensional[D3]         = D3(size, size, size)
    def move(delta: Dimensional[D3]): Cube = Cube(position add delta, size)
    def area: Double                       = ???
    def volume: Double                     = ???
  }

  final case class Triangle[D <: Dimensional[D]](
      position: Dimensional[D],
      aOffset: Dimensional[D],
      bOffset: Dimensional[D],
      cOffset: Dimensional[D]
  ) extends Shaped[D]
      with Surfaced {
    def min: Dimensional[D]                      = ???
    def max: Dimensional[D]                      = ???
    def move(delta: Dimensional[D]): Triangle[D] = ???
    def area: Double                             = ???
    def volume: Double                           = ???
  }
}

object Manipulations {
  import Traits._;
  import Objects._;

  def addDimensions[D <: Dimensional[D]](d1: Dimensional[D], d2: Dimensional[D]): Dimensional[D] =
    (d1, d2) match {
      case (D1(x1), D1(x2))         => D1(x1 + x2).asInstanceOf[Dimensional[D]]
      case (D2(x1, y1), D2(x2, y2)) => D2(x1 + x2, y1 + y2).asInstanceOf[Dimensional[D]]
      case (D3(x1, y1, z1), D3(x2, y2, z2)) =>
        D3(x1 + x2, y1 + y2, z1 + z2).asInstanceOf[Dimensional[D]]
    }

  def negateDimensions[D <: Dimensional[D]](d: Dimensional[D]): Dimensional[D] =
    d match {
      case (D1(x))       => D1(-x).asInstanceOf[Dimensional[D]]
      case (D2(x, y))    => D2(-x, -y).asInstanceOf[Dimensional[D]]
      case (D3(x, y, z)) => D3(-x, -y, -z).asInstanceOf[Dimensional[D]]
    }

  implicit class DimensionOps[D <: Dimensional[D]](value: Dimensional[D]) {
    def negate(): Dimensional[D]                    = negateDimensions(value)
    def add(value2: Dimensional[D]): Dimensional[D] = addDimensions(value, value2)
    def subtract(value2: Dimensional[D]): Dimensional[D] =
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
