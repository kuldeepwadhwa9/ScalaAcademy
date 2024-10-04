class SubstitutionCipher {

  private val cipherMap: Map[Char, Char] = Map(
    'A' -> 'M', 'B' -> 'N', 'C' -> 'O', 'D' -> 'P', 'E' -> 'Q',
    'F' -> 'R', 'G' -> 'S', 'H' -> 'T', 'I' -> 'U', 'J' -> 'V',
    'K' -> 'W', 'L' -> 'X', 'M' -> 'Y', 'N' -> 'Z', 'O' -> 'A',
    'P' -> 'B', 'Q' -> 'C', 'R' -> 'D', 'S' -> 'E', 'T' -> 'F',
    'U' -> 'G', 'V' -> 'H', 'W' -> 'I', 'X' -> 'J', 'Y' -> 'K',
    'Z' -> 'L'
  )

   def encode(plainText: String): String = {
    plainText.map {
      case c if c.isLetter =>
        val upperC: Char = c.toUpper
        cipherMap.getOrElse(upperC, c)
      case c => c
    }
  }

   def decode(cipherText: String): String = {
     val reverseCipherMap: Map[Char, Char] = cipherMap.map(a => a.swap)
    cipherText.map {
      case c if c.isLetter =>
        val upperC = c.toUpper
        reverseCipherMap.getOrElse(upperC, c)
      case c => c
    }
  }

}

object SubstitutionCipher extends SubstitutionCipher
