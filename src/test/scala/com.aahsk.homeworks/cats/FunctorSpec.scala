package com.aahsk.homeworks.cats

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class FunctorSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "functors" should "make sense" in {
    import cats.Functor
    import cats.implicits._

    Functor[Option].map(Option("Hello"))(_.length) should be(
      Some(5)
    )
    Functor[Option].map(None: Option[String])(_.length) should be(
      None
    )

    val lenOption: Option[String] => Option[Int] = Functor[Option].lift(_.length)
    lenOption(Some("Hello")) should be(
      Some(5)
    )

    val source  = List("Cats", "is", "awesome")
    val product = Functor[List].fproduct(source)(_.length).toMap

    product.get("Cats").getOrElse(0) should be(
      4
    )
    product.get("is").getOrElse(0) should be(
      2
    )
    product.get("awesome").getOrElse(0) should be(
      7
    )
  }
}
