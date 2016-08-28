package com.joescii

import java.io.{BufferedReader, InputStream, InputStreamReader}

import scala.util.Try

object PalindromeBuilder {
  def parseInput(is: InputStream): Option[List[(String, String)]] = Try {
    val r = new BufferedReader(new InputStreamReader(is))
    val lines = Iterator.continually(r.readLine()).takeWhile(_ != null).drop(1).toList.zipWithIndex
    val odds = lines.filter(_._2 % 2 == 1).map(_._1)
    val evens = lines.filter(_._2 % 2 == 0).map(_._1)
    evens zip odds
  }.toOption
}
