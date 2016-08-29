package com.joescii

import org.scalacheck.Gen._

object Generators {
  def substringInput = for {
    s <- alphaStr if s.length > 0
    l <- choose(1, s.length)
  } yield (s, l)

  def twoStrings = for {
    a <- alphaStr if a.length > 0
    b <- alphaStr if b.length > 0
  } yield (a, b)
}
