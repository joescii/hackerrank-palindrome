package com.joescii

import org.scalacheck.{ Properties, Prop }
import Prop._
import Generators._

import Solution.palindromes

object PalindromeProperties extends Properties("PalindromeProperties") {
  def isPalindrome(s: String): Boolean = {
    val l = s.length / 2
    s.take(l) == s.takeRight(l).reverse
  }

  property("the returned strings should always be palindromes") = forAll(twoStrings) { case (a, b) =>
    palindromes(a, b).forall(isPalindrome)
  }

}
