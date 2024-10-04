 class CreditCard {

   // Method to validate a credit card number using the Luhn algorithm
   def isValidCardNumber(cardNumber: String): Boolean = {

     // Removing spaces and Convert string to a list of digits and reversing the list to process it from right to left.
     val rmSpacesFromCardNumber = cardNumber.replaceAll(" ", "")

     require(rmSpacesFromCardNumber.forall(_.isDigit), "Card number must be a number")

     // Reverses the array of digits.
     // Handle errors if the user has entered characters - Implement IT
     val digits: Seq[Int] = rmSpacesFromCardNumber.map(_.asDigit).reverse

     // to pair each digit with its index in the reversed array - for eg 1234 -> (1, 0), (2,1), (3,2), (4,3)
     // Map function processes each digit along with its index
     val sum: Int = digits.zipWithIndex.map { case (digit, idx) =>
       // if the index is odd, the digit is doubled
       if (idx % 2 == 1) {
         val doubled = digit * 2

         //if doubled value is greater than 9, we subtract 9 from it
         if (doubled > 9) doubled - 9 else doubled
       } else {
         // if the index is even it is left unchanged
         digit
       }

       //map function processes the entire array and the result is summed
     }.sum

     //Check if the total sum is divisible by 10
     sum % 10 == 0
   }

   // Method to generate a valid credit card number given a prefix and total length
   def generateValidCardNumber(prefix: String, length: Int): String = {

     // Ensure that prefix contains digits, else it will throw exception with message. Require works similar to assert
     // forall is to verify all the elements are digits. Is mainly to verify that a condition holds true for all the elements
     require(prefix.forall(_.isDigit), "Prefix must be numeric.")

     // Ensure length of the prefix is shorter than the total card number length, if it's equal or greater then it will throw and exception
     require(prefix.length < length, "Prefix must be shorter than the total length.")

     // to convert each character in the prefix string to its corresponding digit. "1234" will be [1,2,3,4]
     val prefixDigits = prefix.map(_.asDigit)

     // To generate random digits to fill up the card number.
     // The below generates an array of random digits between 0 and 9. For eg if prefix is "1234" and the total length is
     // 16, we need to generate 16-4-1 random digits
     val randomDigits = Array.fill(length - prefix.length - 1)(scala.util.Random.nextInt(10))

     // Combine prefix and random digits but without the check digit
     val partialNumber = prefixDigits ++ randomDigits
      println("------ partial Number " + partialNumber.mkString)
     // Calculate the check digit using the Luhn algorithm. The check digit will make the card number valid. And converts the array back to string
     val checkDigit = calculateCheckDigit(partialNumber.mkString)

     // Return the complete valid card number. Append the checkDigit to the partialNumber array
     (partialNumber :+ checkDigit).mkString
   }

   // Helper method to calculate the check digit for the Luhn algorithm
   private def calculateCheckDigit(partialCardNumber: String): Int = {
     val rmSpacesFromCardNumber = partialCardNumber.replaceAll(" ", "")
     val digits = rmSpacesFromCardNumber.map(_.asDigit).reverse

     // to pair each digit with its index in the reversed array
     // Map function processes each digit along with its index
     val sum = digits.zipWithIndex.map { case (digit, idx) =>

       // if the index is even, the digit is doubled
       if (idx % 2 == 0) {
         val doubled = digit * 2

         //if doubled value is greater than 9, we subtract 9 from it
         if (doubled > 9) doubled - 9 else doubled
       } else {
         // if the digit is odd it is left unchanged
         digit
       }

       //map function processes the entire array and the result is summed
     }.sum

     // computes the remainder when the sum is divided by 10
     val mod = sum % 10
     println("----- sumDividedBy10 " + mod)
     // for eg, if sumDividedBy10 == 2, the check digit is 10 - 2 = 8. This is to ensure the card number (including the check digit) has a sum divisible by 10
     if (mod == 0) 0 else 10 - mod
   }

 }

   object CreditCard extends CreditCard

