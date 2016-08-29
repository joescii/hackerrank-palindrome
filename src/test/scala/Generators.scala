package com.joescii

import org.scalacheck.Gen._

object Generators {
  def substringInput = for {
    s <- alphaStr if s.length > 0
    l <- choose(1, s.length)
  } yield (s, l)
}
