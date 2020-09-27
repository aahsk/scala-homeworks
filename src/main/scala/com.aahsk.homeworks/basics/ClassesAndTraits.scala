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
  sealed trait Dimensions[D <: Dimensions[D]]

  sealed trait Shape[D <: Dimensions[D]] extends Located[D] with Bounded[D] with Moveable[D]

  sealed trait Located[D <: Dimensions[D]] {
    val position: D
  }

  sealed trait Bounded[D <: Dimensions[D]] {
    def min: D
    def max: D
  }

  sealed trait Moveable[D <: Dimensions[D]] {
    def move(delta: D): Shape[D]
  }
}

object Objects {
  import Traits._;
  import Manipulations._;

  final case class D1(x: Double)                       extends Dimensions[D1]
  final case class D2(x: Double, y: Double)            extends Dimensions[D2]
  final case class D3(x: Double, y: Double, z: Double) extends Dimensions[D3]

  final case class Point[D <: Dimensions[D]](position: D) extends Shape[D] {
    def min                      = position
    def max                      = position
    def move(delta: D): Point[D] = Point(addDimensions(position, delta))
  }

  object OriginD1 extends Located[D1] {
    val position = D1(0)
  }

  object OriginD2 extends Located[D2] {
    val position = D2(0, 0)
  }

  object OriginD3 extends Located[D3] {
    val position = D3(0, 0, 0)
  }

//  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape[Circle] {
//    def x: Double                            = centerX
//    def y: Double                            = centerY
//    def minX: Double                         = centerX - radius
//    def maxX: Double                         = centerX + radius
//    def minY: Double                         = centerY - radius
//    def maxY: Double                         = centerY + radius
//    def move(dx: Double, dy: Double): Circle = Circle(x + dx, y + dy, radius)
//  }

//  final case class Rectangle(
//      var centerX: Double,
//      var centerY: Double,
//      var width: Double,
//      var height: Double
//  ) extends Shape[Rectangle] {
//    def x: Double                               = centerX
//    def y: Double                               = centerY
//    def minX: Double                            = centerX - width / 2
//    def maxX: Double                            = centerX + width / 2
//    def minY: Double                            = centerY - height / 2
//    def maxY: Double                            = centerY + height / 2
//    def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, width, height)
//  }
//
//  final case class Rectangle(
//      var centerX: Double,
//      var centerY: Double,
//      var width: Double,
//      var height: Double
//  ) extends Shape[Rectangle] {
//    def x: Double                               = centerX
//    def y: Double                               = centerY
//    def minX: Double                            = centerX - width / 2
//    def maxX: Double                            = centerX + width / 2
//    def minY: Double                            = centerY - height / 2
//    def maxY: Double                            = centerY + height / 2
//    def move(dx: Double, dy: Double): Rectangle = Rectangle(x + dx, y + dy, width, height)
//  }
}

object Manipulations {
  import Traits._;
  import Objects._;

//  implicit case class DimensionMonuple(monuple: Tuple1[Double]) {
//    implicit def toDimensions(): Dimensions1 = Helpers.dimensions1(monuple._1)
//  }
//
//  implicit case class DimensionTuple(monuple: Tuple1[Double]) {
//    implicit def toDimensions(): Dimensions1 = Helpers.dimensions1(monuple._1)
//  }
//
//  implicit case class DimensionTriple(monuple: Tuple1[Double]) {
//    implicit def toDimensions(): Dimensions1 = Helpers.dimensions1(monuple._1)
//  }

//  final case class Meta[D <: Dimensions[D]]() {
//
//  }

  def addDimensions[D <: Dimensions[D]](d1: Dimensions[D], d2: Dimensions[D]): D =
    (d1, d2) match {
      case (D1(x1), D1(x2))         => D1(x1 + x2).asInstanceOf[D]
      case (D2(x1, y1), D2(x2, y2)) => D2(x1 + x2, y1 + y2).asInstanceOf[D]
      case (D3(x1, y1, z1), D3(x2, y2, z2)) =>
        D3(x1 + x2, y1 + y2, z1 + z2).asInstanceOf[D]
    }

  implicit class DimensionOps[D <: Dimensions[D]](value: Dimensions[D]) {
    def add(value2: Dimensions[D]): Dimensions[D] = addDimensions(value, value2)
  }

//  def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = {
//    new Bounded {
//      implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering
//
//      // if needed, fix the code to be correct
//      override def minX: Double = objects.map(_.minX).min
//      override def maxX: Double = objects.map(_.maxX).max
//      override def minY: Double = objects.map(_.minY).min
//      override def maxY: Double = objects.map(_.maxY).max
//    }
//  }
//
//  implicit class BoundedOps[A <: Set[Bounded]](var objects: A) {
//    def minimumBoundingRectangle(): Bounded = Ops.minimumBoundingRectangle(objects)
//  }
}
