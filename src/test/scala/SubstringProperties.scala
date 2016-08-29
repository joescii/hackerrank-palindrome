package com.joescii

import org.scalacheck.{Gen, Prop, Properties}
import Prop._

import Solution._

object SubstringProperties extends Properties("substrings()") {
  import Generators._

  property("substrings with length 1 should return a List of each character of the string") = forAll(substringInput) { case (s, l) =>
    substrings(s, 1).mkString == s
  }

  property("every string returned should have the specified length") = forAll(substringInput) { case (s, l) =>
    substrings(s, l).map(_.length).find(_ != l) == None
  }

  property("the first character of every substring should form the beginning of original string") = forAll(substringInput) { case (s, l) =>
    s.startsWith(substrings(s, l).map(_.take(1)).mkString)
  }

  property("the last character of every substring should form the end of original string") = forAll(substringInput) { case (s, l) =>
    s.endsWith(substrings(s, l).map(_.takeRight(1)).mkString)
  }

  property("every string returned should be a substring of the original") = forAll(substringInput) { case (s, l) =>
    substrings(s, l).forall(s.contains)
  }

  property("substrings with max length should return a list containing only the string") = forAll(substringInput) { case (s, l) =>
    substrings(s, s.length).toList == List(s)
  }
}
