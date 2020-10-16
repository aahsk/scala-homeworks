package com.aahsk.homeworks.error_handling

import java.time.{LocalDate}
import util.Try
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.syntax.all._

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `CreditCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `CreditCard` instance from the supplied raw data.
object ErrorHandling {

  /**
    * Cardholder's name creation validation errors
    */
  sealed trait CardholderNameError
  object CardholderNameError {
    final case object NameEmpty             extends CardholderNameError;
    final case object NameNotUpperCaseASCII extends CardholderNameError;
  }

  /**
    * Create cardholder's name with uppercase ASCII characters
    * Source: https://stackoverflow.com/a/2005287
    * _Imagine a world where payment method validation is implemented_
    * _using StackOverflow answers as a standard_
    */
  case class CardholderName private (value: List[String])
  object CardholderName {
    def create(value: String): ValidatedNec[CardholderNameError, CardholderName] = {
      import CardholderNameError._

      def validateNameExists: ValidatedNec[CardholderNameError, List[String]] =
        if (value.trim != "") value.split(' ').toList.validNec
        else NameEmpty.invalidNec

      def validateNameUppercase(
          values: List[String]
      ): ValidatedNec[CardholderNameError, CardholderName] = {
        val incorrects = values.filter(!_.matches("^[\\sA-Z]+$"))
        if (incorrects.isEmpty) CardholderName(values).validNec
        else NameNotUpperCaseASCII.invalidNec
      }

      validateNameExists.andThen(validateNameUppercase)
    }
  }

  /**
    * Create PAN with 10-19 digits
    * Source: https://en.wikipedia.org/wiki/Payment_card_number#Structure
    */
  case class PAN private (value: Seq[Int])
  object PAN {
    def create(value: Seq[Int]): Option[PAN] = {
      if (value.length >= 10 && value.length <= 19) Some(PAN(value))
      else None
    }
  }

  /**
    * Create CVC1 with 3-4 digits
    * Source: https://stripe.com/docs/disputes/prevention/verification#cvc-check
    */
  case class CVC1 private (value: Seq[Int])
  object CVC1 {
    def create(value: Seq[Int]): Option[CVC1] = {
      if (value.length >= 3 && value.length <= 4) Some(CVC1(value))
      else None
    }
  }

  /**
    * Expiration date isn't explicitly validated, because
    * there's no reason why an expired PaymentCard shouldn't
    * be representable.
    */
  case class PaymentCard(
      name: CardholderName,
      number: PAN,
      securityCode: CVC1,
      expirationDate: LocalDate
  )

  sealed trait PaymentCardParserError
  object PaymentCardParserError {
    final case class CardholderNameParseError(value: NonEmptyChain[CardholderNameError])
        extends PaymentCardParserError
    final case object PaymentNumberNotIntegerSequence       extends PaymentCardParserError
    final case object PaymentNumberInvalidLength            extends PaymentCardParserError
    final case object SecurityCodeNotIntegerSequence        extends PaymentCardParserError
    final case object SecurityCodeInvalidLength             extends PaymentCardParserError
    final case class ExpirationDateInvalid(message: String) extends PaymentCardParserError
  }

  object PaymentCardParser {
    type AllErrorsOr[A] = ValidatedNec[PaymentCardParserError, A]
    def validate(
        name: String,
        number: String,
        expirationDate: String,
        securityCode: String
    ): AllErrorsOr[PaymentCard] = {
      import PaymentCardParserError._

      // Validate cardholder name
      def validateCardholderName(
          value: String
      ): ValidatedNec[PaymentCardParserError, CardholderName] = {
        CardholderName.create(value) match {
          case Valid(cardholderName) => cardholderName.validNec
          case Invalid(cardholderNameError) =>
            CardholderNameParseError(cardholderNameError).invalidNec
        }
      }

      // Validate payment card number
      def validatePaymentNumberSequence(
          value: String
      ): ValidatedNec[PaymentCardParserError, Seq[Int]] = {
        val mbyInts = value.map(_.toString.toIntOption)
        if (!mbyInts.exists(_.isEmpty)) mbyInts.map(_.get).validNec
        else PaymentNumberNotIntegerSequence.invalidNec
      }

      def validateNumberContent(
          value: Seq[Int]
      ): ValidatedNec[PaymentCardParserError, PAN] = {
        PAN.create(value) match {
          case Some(pan) => pan.validNec
          case None      => PaymentNumberInvalidLength.invalidNec
        }
      }

      def validateNumber(value: String): ValidatedNec[PaymentCardParserError, PAN] =
        validatePaymentNumberSequence(value).andThen(validateNumberContent)

      // Validate security code
      def validateSecurityCodeSequence(
          value: String
      ): ValidatedNec[PaymentCardParserError, Seq[Int]] = {
        val mbyInts = value.map(_.toString.toIntOption)
        if (!mbyInts.exists(_.isEmpty)) mbyInts.map(_.get).validNec
        else SecurityCodeNotIntegerSequence.invalidNec
      }

      def validateSecurityCodeContent(
          value: Seq[Int]
      ): ValidatedNec[PaymentCardParserError, CVC1] = {
        CVC1.create(value) match {
          case Some(pan) => pan.validNec
          case None      => SecurityCodeInvalidLength.invalidNec
        }
      }

      def validateSecurityCode(value: String): ValidatedNec[PaymentCardParserError, CVC1] =
        validateSecurityCodeSequence(value).andThen(validateSecurityCodeContent)

      // Validate expiration date
      def validateExpirationDate(value: String): ValidatedNec[PaymentCardParserError, LocalDate] =
        Try(LocalDate.parse(value)).toEither match {
          case Right(instant) => instant.validNec
          case Left(error)    => ExpirationDateInvalid(error.getMessage).invalidNec
        }

      // Validate payment card
      (
        validateCardholderName(name),
        validateNumber(number),
        validateSecurityCode(securityCode),
        validateExpirationDate(expirationDate)
      ).mapN(PaymentCard)
    }
  }
}
