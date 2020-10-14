package errorhandling


import java.text.SimpleDateFormat
import java.util.Date


import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object ErrorHandlingHomework  {


  import cats.data.ValidatedNec
  import cats.syntax.all._



  // Homework. Place the solution under `error_handling` package in your homework repository.
  //
  // 1. Model `CreditCard` class as an ADT (protect against invalid data as much as it makes sense).
  // 2. Add `ValidationError` cases (at least 5, may be more).
  // 3. Implement `validate` method to construct `CreditCard` instance from the supplied raw data.


    sealed trait ValidationError
    object ValidationError {
      final case object CardholderNameEmpty extends ValidationError {
        override def toString: String = "You should enter cardholder's name!"
      }
      final case object CardNumberContentValidationError extends ValidationError {
        override def toString: String = "Card number contains invalid characters!"
      }
      final case object CardNumberLengthValidationError extends ValidationError {
        override def toString: String = "Credit card number should contain 16 characters!"
      }
      final case object ExpirationDateContentsValidationError extends ValidationError {
        override def toString: String = "Date contains invalid characters!"
      }
      final case object ExpirationDateBoundsValidationError extends ValidationError {
        override def toString: String = "Expiration date is outside the bounds!"
      }
      final case object SecurityCodeContentsValidationError extends ValidationError {
        override def toString: String = "Security code can contain only digits"
      }
      final case object SecurityCodeLengthError extends ValidationError {
        override def toString: String = "Security code must contain 3 digits!"
      }
    }


    case class CreditCard(cardholderName: String, cardNumber: String , expirationDate: Date ,securityCode:String)

    object CreditCardValidator {
      import ValidationError._

      type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

      def validateName(name: String): AllErrorsOr[String] = {
        if (name.length >0) name.validNec
        else CardholderNameEmpty.invalidNec
      }

      def validateCardNumber(cardNumber: String): AllErrorsOr[String] = {
        def validateCardNumberContents: AllErrorsOr[String] = {
          if (cardNumber.forall(x=> x.isDigit)) cardNumber.validNec
          else CardNumberContentValidationError.invalidNec
        }

        def validateCardNumberLength: AllErrorsOr[String] = {
          if (cardNumber.length == 16) cardNumber.validNec
          else CardNumberLengthValidationError.invalidNec
        }

        validateCardNumberContents productR validateCardNumberLength
      }

      def validateExpirationDate(expirationDate: String): AllErrorsOr[Date] = {
        def validateExpirationDateContents: AllErrorsOr[Date] = {
          val simpleDateFormat = new SimpleDateFormat("MM/yy")
          simpleDateFormat.setLenient(false)

          Try(simpleDateFormat.parse(expirationDate)) match {
            case Success(date) => date.validNec
            case Failure(_) => ExpirationDateContentsValidationError.invalidNec
          }
        }

        def validateExpirationDateBounds(expirationDate: Date): AllErrorsOr[Date] = {
          expirationDate.after(new Date) match {
            case true => expirationDate.validNec
            case false => ExpirationDateBoundsValidationError.invalidNec
          }
        }

        validateExpirationDateContents andThen validateExpirationDateBounds
      }

      def validateSecurityCode(securityCode: String): AllErrorsOr[String] = {
        def validateSecurityCodeContents: AllErrorsOr[String] = {
          if (securityCode.forall(x=> x.isDigit)) securityCode.validNec
          else  SecurityCodeContentsValidationError.invalidNec
        }
        def validateSecurityCodeLength: AllErrorsOr[String] = {
          if(securityCode.length == 3) securityCode.validNec
          else SecurityCodeLengthError.invalidNec
        }

        validateSecurityCodeContents productR validateSecurityCodeLength
      }

      def validate(
                    name: String,
                    number: String,
                    expirationDate: String,
                    securityCode: String,
                  ): AllErrorsOr[CreditCard] = {
        (validateName(name) , validateCardNumber(number) , validateExpirationDate(expirationDate) , validateSecurityCode(securityCode)).mapN(CreditCard)
      }
    }

  // Attributions and useful links:
  // https://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html#error-handling
  // https://www.geeksforgeeks.org/scala-exception-handling/
  // https://typelevel.org/cats/datatypes/validated.html
  // https://blog.ssanj.net/posts/2019-08-18-using-validated-for-error-accumulation-in-scala-with-cats.html
}
