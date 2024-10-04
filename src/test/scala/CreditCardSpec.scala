import CreditCard._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CreditCardSpec extends AnyWordSpec with Matchers {

  "isValidCreditCardNumber" should {
    "return true for valid card numbers" in {
      val validCardNumber = "4532015112830366"
      isValidCardNumber(validCardNumber) shouldBe true
    }

    "return false for invalid card numbers" in {
      val invalidCardNumber = "4532015112830367"
      isValidCardNumber(invalidCardNumber) shouldBe false
    }

    "return false for a card number with invalid characters" in {
      val invalidCardNumber = "4532a151128303Z7"
      assertThrows[IllegalArgumentException] {
        isValidCardNumber(invalidCardNumber)
      }
    }

    "return true for valid card numbers with spaces " in {
      val validCardNumberWithSpaces = "4532 0151 1283 0366"
      isValidCardNumber(validCardNumberWithSpaces) shouldBe true
    }

    "return false for a very short invalid card number" in {
      val shortCardNumber = "4532"
      isValidCardNumber(shortCardNumber) shouldBe false
    }

    "generate a valid card number with a given prefix" in {
      val prefix = "1234"
      val length = 16
      val generatedCardNumber =  generateValidCardNumber(prefix, length)

      generatedCardNumber.length shouldBe(length)
      generatedCardNumber.startsWith(prefix) shouldBe true
      isValidCardNumber(generatedCardNumber) shouldBe true
    }

    "throw Exception when prefix is too long" in {
      val longPrefix = "12345678910111213"
      assertThrows[IllegalArgumentException] {
        generateValidCardNumber(longPrefix, 16)
      }
    }

    "throw Exception for non numeric prefix" in {
      val invalidPrefix = "12345a789b1cd11213"
      assertThrows[IllegalArgumentException] {
        generateValidCardNumber(invalidPrefix, 16)
      }
    }
  }
  }
