package com.aahsk.homeworks.basics

import com.sun.tools.javac.code.TypeTag

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe.typeOf

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
  import Objects._
  import Manipulations._

  sealed trait Dimensional[D <: Dimensional[D]] {
    def get: D
  }

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

  sealed trait Moveable[D <: Dimensional[D]] {
    def move(delta: Dimensional[D]): Shaped[D]
  }

  sealed trait LocatedAndSymmetric[D <: Dimensional[D]] extends Located[D] with Bounded[D] {
    val boundSize: Dimensional[D]
    def min: Dimensional[D] = position subtract boundSize
    def max: Dimensional[D] = position add boundSize
  }

  sealed trait Cubelike extends LocatedAndSymmetric[D3] with Surfaced with Volumed {
    def area: Double = {
      val a = boundSize.get.x * boundSize.get.y * 4
      val b = boundSize.get.x * boundSize.get.z * 4
      val c = boundSize.get.y * boundSize.get.z * 4

      a * 2 + b * 2 + c * 2
    }
    def volume: Double = boundSize.get.x * boundSize.get.y * boundSize.get.z
  }
}

object Objects {
  import Traits._;
  import Manipulations._;

  final case class D1(x: Double = 0) extends Dimensional[D1] {
    def get: D1 = this
  }

  final case class D2(x: Double = 0, y: Double = 0) extends Dimensional[D2] {
    def get: D2 = this
  }

  final case class D3(x: Double = 0, y: Double = 0, z: Double = 0) extends Dimensional[D3] {
    def get: D3 = this
  }

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

  final case class Sphere(center: Dimensional[D3], radius: Double)
      extends Shaped[D3]
      with LocatedAndSymmetric[D3]
      with Surfaced
      with Volumed {
    val position: Dimensional[D3]            = center
    val boundSize: Dimensional[D3]           = D3(radius, radius, radius)
    def move(delta: Dimensional[D3]): Sphere = Sphere(center add delta, radius)
    def area: Double                         = 4 * Math.PI * Math.pow(radius, 2)
    def volume: Double                       = ((4 / 3) * Math.PI * Math.pow(radius, 3))
  }

  final case class Rectangle(position: Dimensional[D2], boundSize: Dimensional[D2])
      extends Shaped[D2]
      with LocatedAndSymmetric[D2]
      with Surfaced {
    def move(delta: Dimensional[D2]): Rectangle = Rectangle(position add delta, boundSize)
    def area: Double                            = boundSize.get.x * boundSize.get.y * 4
  }

  final case class Cuboid(position: Dimensional[D3], boundSize: Dimensional[D3])
      extends Shaped[D3]
      with Cubelike {
    def move(delta: Dimensional[D3]): Cuboid = Cuboid(position add delta, boundSize)
  }

  final case class Cube(position: Dimensional[D3], size: Double) extends Shaped[D3] with Cubelike {
    val boundSize: Dimensional[D3]         = D3(size, size, size)
    def move(delta: Dimensional[D3]): Cube = Cube(position add delta, size)
  }

  final case class TriangleD3(
      position: Dimensional[D3],
      aOffset: Dimensional[D3],
      bOffset: Dimensional[D3],
      cOffset: Dimensional[D3]
  ) extends Shaped[D3]
      with Surfaced {
    def min: Dimensional[D3] = ???
    def max: Dimensional[D3] = ???
    def move(delta: Dimensional[D3]): TriangleD3 =
      TriangleD3(position add delta, aOffset, bOffset, cOffset)
    def area: Double   = ???
    def volume: Double = ???
  }
}

object Manipulations {
  import Traits._;
  import Objects._;

  // WARNING: This doesn't type protect against adding different dimensions
  // WARNING: Abuses the type system by use of `asInstanceOf`
  def addDimensions[D <: Dimensional[D]](
      d1: Dimensional[D],
      d2: Dimensional[D]
  ): Dimensional[D] =
    (d1, d2) match {
      case (D1(x1), D1(x2))         => D1(x1 + x2).asInstanceOf[Dimensional[D]]
      case (D2(x1, y1), D2(x2, y2)) => D2(x1 + x2, y1 + y2).asInstanceOf[Dimensional[D]]
      case (D3(x1, y1, z1), D3(x2, y2, z2)) =>
        D3(x1 + x2, y1 + y2, z1 + z2).asInstanceOf[Dimensional[D]]
    }

  // WARNING: Abuses the type system by use of `asInstanceOf`
  def negateDimensions[D <: Dimensional[D]](d: Dimensional[D]): Dimensional[D] =
    d match {
      case D1(x)       => D1(-x).asInstanceOf[Dimensional[D]]
      case D2(x, y)    => D2(-x, -y).asInstanceOf[Dimensional[D]]
      case D3(x, y, z) => D3(-x, -y, -z).asInstanceOf[Dimensional[D]]
    }

  // WARNING: Uses unsafe methods `negateDimensions` and `addDimensions`
  implicit class DimensionOps[D <: Dimensional[D]](value: Dimensional[D]) {
    def negate(): Dimensional[D]                    = negateDimensions(value)
    def add(value2: Dimensional[D]): Dimensional[D] = addDimensions(value, value2)
    def subtract(value2: Dimensional[D]): Dimensional[D] =
      addDimensions(value, negateDimensions(value2))
  }

  def minimumBoundD1(objects: Set[Bounded[D1]]): Bounded[D1] = {
    val minx = objects.map(_.min.get.x).min
    val maxx = objects.map(_.max.get.x).min

    new Bounded[D1] {
      def min: Dimensional[D1] = D1(minx)
      def max: Dimensional[D1] = D1(maxx)
    }
  }

  def minimumBoundD2(objects: Set[Bounded[D2]]): Bounded[D2] = {
    val minx = objects.map(_.min.get.x).min
    val miny = objects.map(_.min.get.y).min
    val maxx = objects.map(_.max.get.x).max
    val maxy = objects.map(_.max.get.y).max

    new Bounded[D2] {
      def min: Dimensional[D2] = D2(minx, miny)
      def max: Dimensional[D2] = D2(maxx, maxy)
    }
  }

  def minimumBoundD3(objects: Set[Bounded[D3]]): Bounded[D3] = {
    val minx = objects.map(_.min.get.x).min
    val miny = objects.map(_.min.get.y).min
    val minz = objects.map(_.min.get.z).min
    val maxx = objects.map(_.max.get.x).max
    val maxy = objects.map(_.max.get.y).max
    val maxz = objects.map(_.max.get.z).max

    new Bounded[D3] {
      def min: Dimensional[D3] = D3(minx, miny, minz)
      def max: Dimensional[D3] = D3(maxx, maxy, maxz)
    }
  }

  // WARNING: Abuses the type system by use of `asInstanceOf`
  def minimumBound[D <: Dimensional[D]: ClassTag](
      dimensionalObjects: Set[Bounded[D]]
  )(implicit tag: ClassTag[Set[Bounded[D]]]): Bounded[D] =
    dimensionalObjects match {
      case objects: Set[Bounded[D1] @unchecked] if classTag[D] == classTag[D1] =>
        minimumBoundD1(objects).asInstanceOf[Bounded[D]]
      case objects: Set[Bounded[D2] @unchecked] if classTag[D] == classTag[D2] =>
        minimumBoundD2(objects).asInstanceOf[Bounded[D]]
      case objects: Set[Bounded[D3] @unchecked] if classTag[D] == classTag[D3] =>
        minimumBoundD3(objects).asInstanceOf[Bounded[D]]
    }

  implicit class BoundedOps[D <: Dimensional[D]: ClassTag](var objects: Set[Bounded[D]])(implicit
      tag: ClassTag[Set[Bounded[D]]]
  ) {
    def minimumBoundingRectangle(): Bounded[D] =
      Manipulations.minimumBound(objects)
  }
}
