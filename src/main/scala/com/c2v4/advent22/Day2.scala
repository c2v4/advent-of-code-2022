package com.c2v4.advent22

import scala.io.Source
import scala.util.Using


@main def main2() = {

  Using(Source.fromFile("2.txt")) {
    input =>
      val strings = input.mkString.split("\n")
      val summed = strings.map(toPoints).sum
      println(summed)
      println(strings.map(toPoints2).sum)
  }

  def toPoints(str: String): Int = {
    val round = str.split(" ")
    val opp = round(0).charAt(0) - 'A'
    val my = round(1).charAt(0) - 'X'
    val result = (my - opp) match
      case -2 => 6
      case -1 => 0
      case 0 => 3
      case 1 => 6
      case 2 => 0
    result + scoreForChoice(my)
  }

  def toPoints2(str: String): Int = {
    val round = str.split(" ")
    val opp = round(0).charAt(0) - 'A'
    val my = round(1).charAt(0) - 'X'
    my*3+combined(opp,my)
  }

  def scoreForChoice(choice: Int): Int = choice + 1

  def combined(opp: Int, my: Int): Int ={
    (opp,my) match
      case (0,0) => 3
      case (0,1) => 1
      case (0,2) => 2
      case (1,0) => 1
      case (1,1) => 2
      case (1,2) => 3
      case (2,0) => 2
      case (2,1) => 3
      case (2,2) => 1
  }
}
