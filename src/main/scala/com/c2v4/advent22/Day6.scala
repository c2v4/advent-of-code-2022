package com.c2v4.advent22

import scala.annotation.tailrec
import scala.collection.immutable.{HashMap, Queue}
import scala.io.Source
import scala.util.Using


@main def main6() = {

  Using(Source.fromFile("6.txt")) {
    input =>
      val string = input.mkString
      println(findIndex(string,4))
      println(findIndex(string,14))

  }

  // finds the first position where the four most recently received characters were all different
  @tailrec
  def findIndex(s: String, consecutiveCharacters:Int, i: Int = 0, lastCharacters:Queue[Char] = Queue.empty): Int = {
    if (i >= s.length) -1
    else {
      if (lastCharacters.size == consecutiveCharacters) {
        if (lastCharacters.distinct.size == consecutiveCharacters) {
          i
        } else {
          findIndex(s, consecutiveCharacters, i + 1, lastCharacters.tail.enqueue(s(i)))
        }
      } else {
        findIndex(s, consecutiveCharacters, i + 1, lastCharacters.enqueue(s(i)))
      }

    }
  }

}