package com.aahsk.homeworks.error_handling

import cats.data.Validated.{Invalid, Valid}
import com.aahsk.homeworks.error_handling.ErrorHandling._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

/**
  * Truth be told - this spec should be way larger, but
  * I honestly just wrote it to check that this compiles
  * not to actually cover all edge cases etc.
  */
class ErrorHandlingSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks {
  "PaymentCardParser.validate" should "error on an incorrect card" in {
    val emptyCard = PaymentCardParser.validate(
      "",
      "",
      "",
      ""
    )

    // The expected errors are something along the lines of:
    //   Invalid(Chain(
    //     CardholderNameParseError(Chain(NameEmpty)),
    //     PaymentNumberInvalidLength,
    //     SecurityCodeInvalidLength,
    //     ExpirationDateInvalid(Text '' could not be parsed at index 0)
    //   ))
    // That said... I've no idea how to check that a chain contains types
    // I guess I could pattern match, but that would end up looking messy
    // Instead I just check that the validation failed w/ more than 4 errors
    emptyCard match {
      case Valid(_)   => throw new Error("Card validation didn't work")
      case Invalid(e) => assert(e.length >= 4)
    }
  }

  "PaymentCardParser.validate" should "be able to parse and create payment card" in {
    val card = PaymentCardParser.validate(
      "JOHN DOE",
      "1234567890",
      "2020-12-12",
      "123"
    )
    assert(card.isValid)
  }
}
