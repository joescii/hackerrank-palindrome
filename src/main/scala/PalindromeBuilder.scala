package com.joescii

import java.io.{BufferedReader, InputStream, InputStreamReader}

import scala.util.Try

object PalindromeBuilder extends App {
  def parseInput(is: InputStream): Option[List[(String, String)]] = Try {
    val r = new BufferedReader(new InputStreamReader(is))
    val lines = Iterator.continually(r.readLine()).takeWhile(_ != null).drop(1).toList.zipWithIndex
    val odds = lines.filter(_._2 % 2 == 1).map(_._1)
    val evens = lines.filter(_._2 % 2 == 0).map(_._1)
    evens zip odds
  }.toOption

  type Substring = (Option[Char], String, Option[Char])
  implicit class SubstringOps(val s: Substring) extends AnyVal {
    def === (other:Substring): Boolean = s._2 == other._2
    def str = s._2
    def ahead = s._1
    def behind = s._3
  }

  def substrings(s: String, l: Int): List[Substring] = (0 to (s.length - l))
    .map(i => (s.lift(i - 1), s.drop(i).take(l), s.lift(i + l)))
    .toList

  def crossProduct[A, B](a: List[A], b: List[B]): List[(A, B)] = for { i <- a; j <- b } yield (i, j)

  def palindromes(a: String, b: String): List[String] = {
    val startLength = Math.min(a.length, b.length)

    Stream.from(startLength, -1).takeWhile(_ > 0)
      .map(length => crossProduct(substrings(a, length), substrings(b.reverse, length)))
      .map(strs => strs.filter { case (a, b) => a === b })
      .find(!_.isEmpty)
      .map(_.map { case ((_, a, _), (_, b, _)) =>
        a + b.reverse
      })
      .getOrElse(List())
  }

  val input = parseInput(System.in)
  input.map(_.map { case (a, b) => System.out.println(
    palindromes(a, b).sorted.headOption.getOrElse("-1")
  )})
}
