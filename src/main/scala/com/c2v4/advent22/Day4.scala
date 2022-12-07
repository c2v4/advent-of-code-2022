package com.c2v4.advent22

import scala.io.Source
import scala.util.Using


@main def main4() = {

  Using(Source.fromFile("4.txt")) {
    input =>
      val strings = input.mkString.split("\n")
      println(strings.count(covered))
      println(strings.count(overlap))
  }

  def covered(str: String): Boolean = {
    val strings = str.split(",")
    val x1 = strings(0).split("-")(0).toInt
    val y1 = strings(0).split("-")(1).toInt
    val x2 = strings(1).split("-")(0).toInt
    val y2 = strings(1).split("-")(1).toInt
    x1 == x2 || y1 == y2 || Math.signum(x1 - x2) == Math.signum(y2 - y1)
  }

  def overlap(str: String): Boolean = {
    val strings = str.split(",")
    val x1 = strings(0).split("-")(0).toInt
    val y1 = strings(0).split("-")(1).toInt
    val x2 = strings(1).split("-")(0).toInt
    val y2 = strings(1).split("-")(1).toInt
    val bool =  y2 >= x1 && y1 >= x2
    bool
  }

}