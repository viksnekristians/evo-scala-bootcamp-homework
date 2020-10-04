package basics

import basics.ControlStructuresHomework.Command._

import scala.io.Source
import scala.util.control.Exception.allCatch

object ControlStructuresHomework {
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
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  sealed trait Result {
    def resultText: String
  }
  final case class SuccessfulResult(command: Command , result: Double) extends Result {
    def resultText: String =
      command match {
        case d:Divide => s"${d.dividend} divided by ${d.divisor} is $result"
        case s:Sum => s"the sum of ${s.numbers.mkString(" ")} is $result"
        case a:Average => s"the average of ${a.numbers.mkString(" ")} is $result"
        case min:Min => s"the minimum of ${min.numbers.mkString(" ")} is $result"
        case max:Max => s"the maximum of ${max.numbers.mkString(" ")} is $result"
        case _ => "Error: Unknown error!"
      }
  } // adjust Result as required to match requirements

  final case class ErrorResult(error: ErrorMessage) extends Result {
    def resultText: String =  s"Error: ${error.value}"
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {


    val commandList = x.split(" ").toList

    commandList match {
      case x::xs =>
        x match {
          case "divide" => {
            if (xs.length == 2 && xs.forall(el => (allCatch opt el.toDouble).isDefined)) Right(Divide(xs(0).toDouble, xs(1).toDouble))
            else Left(ErrorMessage("Wrong argument list!"))
          }

          case "sum" => {
            // we could also assume that we can use the Sum function with just one parameter.
            if (xs.length >= 2 && xs.forall(el => (allCatch opt el.toDouble).isDefined)) {
              val numbersList = for (el <- xs) yield el.toDouble
              Right(Sum(numbersList))
            }
            else Left(ErrorMessage("Wrong argument list!"))
          }

          case "average" => {
            if (xs.nonEmpty && xs.forall(el => (allCatch opt el.toDouble).isDefined)) {
              val numbersList = for (el <- xs) yield el.toDouble
              Right(Average(numbersList))
            }
            else Left(ErrorMessage("Wrong argument list!"))
          }

          case "min" => {

            if (xs.nonEmpty && xs.forall(el => (allCatch opt el.toDouble).isDefined)) {
              val numbersList = for (el <- xs) yield el.toDouble
              Right(Min(numbersList))
            }
            else Left(ErrorMessage("Wrong argument list!"))
          }

          case "max" => {

            if (xs.nonEmpty && xs.forall(el => (allCatch opt el.toDouble).isDefined)) {
              val numbersList = for (el <- xs) yield el.toDouble
              Right(Max(numbersList))
            }
            else Left(ErrorMessage("Wrong argument list!"))
          }
          case _ => Left(ErrorMessage("The method you typed is not valid!"))
        }
      case _ => Left(ErrorMessage("Input text invalid!"))
    }
  }

  // Consider how to handle extra whitespace gracefully (without errors).


  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case d:Divide =>  if (d.divisor != 0) Right(SuccessfulResult(d, d.dividend / d.divisor)) else Left(ErrorMessage("Dividing by 0 is not allowed"))
      case s:Sum => Right(SuccessfulResult(s,s.numbers.sum))
      case a:Average => Right(SuccessfulResult(a , a.numbers.sum / a.numbers.length))
      case minimum:Min => Right(SuccessfulResult(minimum ,minimum.numbers.min))
      case maximum:Max => Right(SuccessfulResult(maximum , maximum.numbers.max))
      case _ => Left(ErrorMessage("Unknown error"))
    }
  }


  def renderResult(x: Result): String = {
    x.resultText
  }

  def process(x: String): String = {

    val parsedCommand: Either[ErrorMessage, Command] = parseCommand(x)
    val getObject = parsedCommand.toOption.getOrElse(parsedCommand.left.getOrElse(ErrorMessage("Unknown error!")))
    val result: String  = getObject match {
      case c:Command =>  {
        val calculation = calculate(c)
        val calculatedOrError = calculation.toOption.getOrElse(calculation.left.getOrElse(ErrorMessage("Unknown error!")))
        calculatedOrError match {
          case r:Result => renderResult(r)
          case e:ErrorMessage => renderResult(ErrorResult(e))
        }
      }

      case e:ErrorMessage => renderResult(ErrorResult(e))
      case _ => renderResult(ErrorResult(ErrorMessage("Unknown error!")))
    }

    result

  }


  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
