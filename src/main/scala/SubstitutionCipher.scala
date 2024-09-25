class SubstitutionCipher {

  private val cipherMap: Map[Char, Char] = Map(
    'A' -> 'M', 'B' -> 'N', 'C' -> 'O', 'D' -> 'P', 'E' -> 'Q',
    'F' -> 'R', 'G' -> 'S', 'H' -> 'T', 'I' -> 'U', 'J' -> 'V',
    'K' -> 'W', 'L' -> 'X', 'M' -> 'Y', 'N' -> 'Z', 'O' -> 'A',
    'P' -> 'B', 'Q' -> 'C', 'R' -> 'D', 'S' -> 'E', 'T' -> 'F',
    'U' -> 'G', 'V' -> 'H', 'W' -> 'I', 'X' -> 'J', 'Y' -> 'K',
    'Z' -> 'L'
  )

  private val reverseCipherMap: Map[Char, Char] = cipherMap.map(_.swap)

   def encode(plainText: String): String = {
    plainText.map {
      case c if c.isLetter =>
        val upperC: Char = c.toUpper
        cipherMap.getOrElse(upperC, c)
      case c => c
    }
  }

   def decode(cipherText: String): String = {
    cipherText.map {
      case c if c.isLetter =>
        val upperC = c.toUpper
        reverseCipherMap.getOrElse(upperC, c)
      case c => c
    }
  }

}

object SubstitutionCipher extends SubstitutionCipher {

  private val plainText = "HELLO WORLD"
  private val encodeText = SubstitutionCipher.encode(plainText)
  private val decodeText = SubstitutionCipher.decode(encodeText)

  println(s"Original text " + plainText)
  println(s"Encoded text " + encodeText)
  println(s"Decoded text " + decodeText)

}