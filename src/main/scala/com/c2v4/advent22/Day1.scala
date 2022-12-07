package com.c2v4.advent22

import scala.io.Source
import scala.util.Using


@main def main1() = {

  Using(Source.fromFile("1.txt")) {
    input =>
      val summed = input.mkString.split("\n\n").map(_.split("\n").map(_.toInt).sum)
      println(summed.max)
      println(summed.sorted.reverse.take(3).sum)
  }


}
