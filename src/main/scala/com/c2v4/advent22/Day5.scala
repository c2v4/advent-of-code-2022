package com.c2v4.advent22

import scala.collection.immutable.HashMap
import scala.io.Source
import scala.util.Using


@main def main5() = {

  Using(Source.fromFile("5.txt")) {
    input =>
      val strings = input.mkString.split("\n\n")
      val value = strings(0).split("\n").reverse.drop(1).foldLeft(HashMap.empty[Int, List[Char]]) {
        (acc, s) =>
          Range.inclusive(0, s.length / 4).foldLeft(acc) {
            (acc2, i) =>
              val list = acc2.getOrElse(i, List.empty[Char])
              val c = s(1 + i * 4)
              if (c != ' ') acc2 + (i -> (list :+ c)) else acc2
          }
      }
      val result = strings(1).split("\n").map {
        s =>
          ("""\d+""".r findAllIn s).toList.map(_.toInt)
      }.foldLeft(value) {
        (acc, list) =>
          val amount = list.head
          val from = list(1) - 1
          val to = list(2) - 1
          val toMove = acc.getOrElse(from, List.empty).takeRight(amount).reverse
          val result = acc.updated(from, acc.getOrElse(from, List.empty).dropRight(amount)).updated(to, acc.getOrElse(to, List.empty) ++ toMove)
          result
      }
      val result2 = strings(1).split("\n").map {
        s =>
          ("""\d+""".r findAllIn s).toList.map(_.toInt)
      }.foldLeft(value) {
        (acc, list) =>
          val amount = list.head
          val from = list(1) - 1
          val to = list(2) - 1
          val toMove = acc.getOrElse(from, List.empty).takeRight(amount)
          val result = acc.updated(from, acc.getOrElse(from, List.empty).dropRight(amount)).updated(to, acc.getOrElse(to, List.empty) ++ toMove)
          result
      }
      val str = Range.inclusive(0, result.size - 1).map(i => result.getOrElse(i, List.empty).last).mkString
      val str2 = Range.inclusive(0, result2.size - 1).map(i => result2.getOrElse(i, List.empty).last).mkString
      println(str)
      println(str2)
  }

}