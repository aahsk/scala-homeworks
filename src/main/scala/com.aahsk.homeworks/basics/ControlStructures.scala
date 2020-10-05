package com.aahsk.homeworks.basics

import scala.io.Source

object ControlStructures {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double])                extends Command
    final case class Average(numbers: List[Double])            extends Command
    final case class Min(numbers: List[Double])                extends Command
    final case class Max(numbers: List[Double])                extends Command
  }

  sealed trait Result
  sealed trait ErrorResult             extends Result
  sealed trait SuccessResult           extends Result
  sealed trait ParseError              extends ErrorResult
  sealed trait CalculationError        extends ErrorResult
  sealed trait NumberCalculationResult extends SuccessResult

  final case class DoubleParseError(xs: List[String])          extends ParseError
  final case object MissingCommandError                        extends ParseError
  final case class UnknownCommandError(command: String)        extends ParseError
  final case class ExactArgumentCountError(receivedCount: Int) extends ParseError
  final case object DivisionByZero                             extends CalculationError
  final case object NumberCalculationOfNil                     extends CalculationError
  final case class DivisionResult(a: Double, b: Double, value: Double)
      extends NumberCalculationResult
  final case class SumResult(xs: List[Double], value: Double)     extends NumberCalculationResult
  final case class AverageResult(xs: List[Double], value: Double) extends NumberCalculationResult
  final case class MinResult(xs: List[Double], value: Double)     extends NumberCalculationResult
  final case class MaxResult(xs: List[Double], value: Double)     extends NumberCalculationResult

  def constructNumberCalculationCommand(
      x: String,
      xs: List[Double]
  ): Either[ErrorResult, Command] = {
    import Command._

    (x, xs) match {
      case ("divide", xs) =>
        xs match {
          case a :: b :: Nil => Right(Divide(a, b))
          case xs            => Left(ExactArgumentCountError(xs.length))
        }

      case ("sum" | "average" | "min" | "max", xs) =>
        xs match {
          case xs =>
            x match {
              case "sum"     => Right(Sum(xs))
              case "average" => Right(Average(xs))
              case "min"     => Right(Min(xs))
              case "max"     => Right(Max(xs))
            }
        }

      case (x, _) => Left(UnknownCommandError(x))
    }
  }

  def parseCommand(x: String): Either[ErrorResult, Command] = {
    import cats.implicits._

    def toDoubleEither(x: String): Either[String, Double] = x.toDoubleOption.toRight(x)
    def listOfEithersToEitherOfLists[A, B](xs: List[Either[A, B]]): Either[List[A], List[B]] = {
      val partitioned = xs.partition(_.isLeft)
      partitioned match {
        case (Nil, rights) => Right((for (Right(i) <- rights) yield i))
        case (lefts, _)    => Left((for (Left(i) <- lefts) yield i))
      }
    }

    val tokens = x.split(' ').filterNot(_.isBlank).toList
    tokens match {
      case Nil => Left(MissingCommandError)
      case x :: xs =>
        for {
          xs      <- listOfEithersToEitherOfLists(xs.map(toDoubleEither)).leftMap(DoubleParseError)
          command <- constructNumberCalculationCommand(x, xs)
        } yield command
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorResult, Result] = {
    import Command._
    x match {
      case Divide(_, 0)              => Left(DivisionByZero)
      case Divide(dividend, divisor) => Right(DivisionResult(dividend, divisor, dividend / divisor))
      case Sum(xs)                   => Right(SumResult(xs, xs.sum))
      case Average(Nil)              => Left(NumberCalculationOfNil)
      case Average(xs)               => Right(AverageResult(xs, xs.sum / xs.length))
      case Min(Nil)                  => Left(NumberCalculationOfNil)
      case Min(xs)                   => Right(MinResult(xs, xs.min))
      case Max(Nil)                  => Left(NumberCalculationOfNil)
      case Max(xs)                   => Right(MaxResult(xs, xs.max))
    }
  }

  def renderResult(x: Result): String = {
    def error(message: String) = f"Error: ${message}"
    def listCalculation(calculation: String, xs: List[Double], x: Double) =
      f"The ${calculation} of ${xs.mkString(" ")} is ${x}"
    x match {
      case MissingCommandError          => error("Missing command to execute")
      case UnknownCommandError(command) => error(s"Unknown command '${command}'")
      case ExactArgumentCountError(req) =>
        error(s"This command requires precisely ${req} arguments")
      case DoubleParseError(xs) =>
        error(s"Failed to parse the following numbers: ${xs.mkString(", ")}")
      case DivisionByZero => error("Division by zero is not defined")
      case NumberCalculationOfNil =>
        error("This function isn't defined for an empty list of elements")
      case DivisionResult(a, b, x) => s"${a} divided by ${b} is ${x}"
      case SumResult(xs, x)        => listCalculation("sum", xs, x)
      case AverageResult(xs, x)    => listCalculation("average", xs, x)
      case MinResult(xs, x)        => listCalculation("minimum", xs, x)
      case MaxResult(xs, x)        => listCalculation("maximum", xs, x)
    }
  }

  def process(x: String): String = {
    (for {
      command <- parseCommand(x)
      result  <- calculate(command)
    } yield result).fold(renderResult, renderResult)
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
