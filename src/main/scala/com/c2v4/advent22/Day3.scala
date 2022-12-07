package com.c2v4.advent22

import scala.io.Source
import scala.util.Using


@main def main3() = {

  Using(Source.fromFile("3.txt")) {
    input =>
      val strings = input.mkString.split("\n")
      println(strings.map(str => letterToValue(letterInCommon(str.substring(0, str.length / 2), str.substring(str.length / 2)))).sum)
      println(strings.grouped(3).map(str => letterToValue(letterInCommon(str(0), str(1), str(2)))).sum)
  }

  def letterInCommon(strings: String*): Char = {
    strings.drop(1).fold(strings.head)((acc, str) => acc.intersect(str)).head
  }

  def letterToValue(c: Char): Int =
    c.toInt - (if (c.isLower) {
      96
    } else {
      65 - 27
    }
      )

}