import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SubstitutionCipherSpec extends AnyWordSpec with Matchers {

  "SubstitutionCipher" should {
    "encode plain text correctly" in {
      val plainText = "HELLO"
      val encodedText = SubstitutionCipher.encode(plainText)
      encodedText mustBe "TQXXA"
    }

    "decode cipher text correctly" in {
      val cipherText = "TQXXA"
      val decodedText = SubstitutionCipher.decode(cipherText)
      decodedText mustBe "HELLO"
    }

    "ignore non-alphabetic characters" in {
      val plainText = "HELLO WORLD!"
      val encodedText = SubstitutionCipher.encode(plainText)
      encodedText mustBe "TQXXA IADXP!"
    }

    "handle case insensitivity" in {
      val plainText = "hello"
      val encodedText = SubstitutionCipher.encode(plainText)
      encodedText mustBe "TQXXA"
    }
  }

  }
