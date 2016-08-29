package com.joescii

import java.io.{BufferedReader, InputStream, InputStreamReader}

import scala.util.Try

object Solution extends App {
  def parseInput(is: InputStream): Option[List[(String, String)]] = Try {
    val r = new BufferedReader(new InputStreamReader(is))
    val lines = Iterator.continually(r.readLine()).takeWhile(_ != null).drop(1).toList.zipWithIndex
    val (evens, odds) = lines.partition(_._2 % 2 == 0)
    evens.map(_._1) zip odds.map(_._1)
  }.toOption

  def substrings(s: String, l: Int): TraversableOnce[String] = Stream.from(0).takeWhile(_ <= s.length - l)
    .map(i => s.drop(i).take(l))

  def crossProduct[A, B](a: TraversableOnce[A], b: TraversableOnce[B]): TraversableOnce[(A, B)] = for { i <- a; j <- b } yield (i, j)

  def isPalindrome(s: String): Boolean = s == s.reverse

  def palindromes(a: String, b: String): List[String] = {
    val startLength = a.length + b.length

    Iterator.from(startLength, -1).takeWhile(_ > 0)
      .map(length => crossProduct(substrings(a, length), substrings(b.reverse, length)))
      .find(!_.isEmpty)
      .map(_.map { case (a, b) =>
          a + b.reverse
        }.toList.sorted
      )
      .getOrElse(List())
  }

  val input = parseInput(System.in)
  input.map(_.map { case (a, b) => System.out.println(
    palindromes(a, b).sorted.headOption.getOrElse("-1")
  )})
}
