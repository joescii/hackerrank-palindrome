package com.joescii

import org.scalatest.{FlatSpec, ShouldMatchers}

class InputSpec extends FlatSpec with ShouldMatchers {
  "parseInput()" should "handle the sample from the problem" in {
    val is = this.getClass.getClassLoader.getResourceAsStream("input.txt")

    Solution.parseInput(is) shouldEqual Some(List(("bac", "bac"), ("abc", "def"), ("jdfh", "fds")))
  }
}
