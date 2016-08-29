package com.joescii

import org.scalacheck.{Gen, Prop, Properties}
import Prop._

import Solution._

object SubstringProperties extends Properties("substrings()") {
  import Generators._

  property("substrings with length 1 should return a List of each character of the string") = forAll(substringInput) { case (s, l) =>
    substrings(s, 1).map(_.str).mkString == s
  }

  property("every string returned should have the specified length") = forAll(substringInput) { case (s, l) =>
    substrings(s, l).map(_.str.length).find(_ != l) == None
  }

  property("the first character of every substring should form the beginning of original string") = forAll(substringInput) { case (s, l) =>
    s.startsWith(substrings(s, l).map(_.str.take(1)).mkString)
  }

  property("the last character of every substring should form the end of original string") = forAll(substringInput) { case (s, l) =>
    s.endsWith(substrings(s, l).map(_.str.takeRight(1)).mkString)
  }

  property("every string returned should be a substring of the original") = forAll(substringInput) { case (s, l) =>
    substrings(s, l).map(_.str).forall(s.contains)
  }

  property("substrings with max length should return a list containing only the string") = forAll(substringInput) { case (s, l) =>
    substrings(s, s.length).map(_.str).toList == List(s)
  }

  property("all substrings except the last will have behind defined") = forAll(substringInput) { case (s, l) =>
    val init = substrings(s, l).toList.init
    init.filter(_.behind.isDefined) == init
  }

  property("the last substring will never have behind defined") = forAll(substringInput) { case (s, l) =>
    substrings(s, l).toList.last.behind == None
  }

  property("all of the 'behinds' should define a suffix of the original string") = forAll(substringInput) { case (s, l) =>
    s.endsWith(substrings(s, l).flatMap(_.behind).mkString)
  }
}
