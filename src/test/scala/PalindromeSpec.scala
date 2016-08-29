package com.joescii

import org.scalatest.{ShouldMatchers, WordSpec}

import Solution._

class PalindromeSpec extends WordSpec with ShouldMatchers {
  "palindrome()" should {
    "find 'aba' from 'bac', 'bac'" in {
      palindromes("bac", "bac") shouldEqual List("aba", "bab", "cac")
    }

    "find nothing from 'abc', 'def'" in {
      palindromes("abc", "def") shouldEqual List()
    }

    "find 'dfhfd' from 'jdfh', 'fds'" in {
      palindromes("jdfh", "fds") shouldEqual List("dfhfd")
    }

    "find 'abccba' from 'abc', 'cba'" in {
      palindromes("abc", "cba") shouldEqual List("abccba")
    }

    "find 'aca' from 'bac', 'ac'" in {
      palindromes("bac", "ac") shouldEqual List("aca", "cac")
    }

    "find 'cc' from 'c', 'c'" in {
      palindromes("c", "c") shouldEqual List("cc")
    }

    "find 'cc' from 'stuffc', 'c'" in {
      palindromes("stuffc", "c") shouldEqual List("cc")
    }

    "complete a palindrome" in {
      palindromes("abcdedcb", "a") shouldEqual List("abcdedcba")
    }

    "return a palindrome if that is the input" in {
      palindromes("abcba", "def") shouldEqual List("abcba")
    }

    "return a palindrome if it is in the input and the other argument contributes nothing to it" in {
      palindromes("juabcbank", "def") shouldEqual List("abcba")
    }
  }
}
