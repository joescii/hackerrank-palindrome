package com.joescii

import org.scalatest.{ShouldMatchers, WordSpec}

import PalindromeBuilder._

class PalindromeSpec extends WordSpec with ShouldMatchers {
  "palindrome()" should {
    "find 'aba' from 'bac', 'bac'" in {
      palindromes("bac", "bac") shouldEqual List("aba")
    }

    "find nothing from 'abc', 'def'" in {
      palindromes("abc", "def") shouldEqual List()
    }

    "find 'dfhfd' from 'jdfh', 'fds'" in {
      palindromes("jdfh", "fds") shouldEqual List("dfhfd")
    }
  }
}
