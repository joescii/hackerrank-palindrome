package com.joescii

import org.scalacheck.{Gen, Prop, Properties}
import Prop._
import Gen._

import PalindromeBuilder.crossProduct

object CrossProductProperties extends Properties("crossProduct()") {
  property("crossProduct() should produce a list with length of the product of input lengths") =
    forAll(nonEmptyListOf(alphaStr), nonEmptyListOf(alphaStr)) { (a, b) =>
      crossProduct(a, b).length == a.length * b.length
    }

  property("crossProduct().map(_._1) should have the same elements as the input") =
    forAll(nonEmptyListOf(alphaStr), nonEmptyListOf(alphaStr)) { (a, b) =>
      crossProduct(a, b).map(_._1).toSet == a.toSet
    }

  property("crossProduct().map(_._2) should have the same elements as the input") =
    forAll(nonEmptyListOf(alphaStr), nonEmptyListOf(alphaStr)) { (a, b) =>
      crossProduct(a, b).map(_._2).toSet == b.toSet
    }
}
