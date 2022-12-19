package com.c2v4.advent22

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day8 {

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("8.txt")) {
      input =>
        val treeMap = input.mkString.split("\n").map(a => a.toCharArray.map(_ - '0'))
        println(solve(treeMap))
        println(solve2(treeMap))
    }
  }


  def solve2(array: Array[Array[Int]]): Int = {
    convertToScore(array).map(_.max).max
  }


  def convertToScore(array: Array[Array[Int]]): Array[Array[Int]] = {

    enum Direction {
      case Up, Down, Left, Right
    }

    def visibilityFor(x: Int, y: Int, height: Int, direction: Direction): Int = {
      direction match {
        case Direction.Up => {
          if (x == 0) 0 else if (height <= array(x - 1)(y)) 1
          else visibilityFor(x - 1, y, height, Direction.Up) + 1
        }
        case Direction.Down
        => {
          if (x == array.length - 1) 0
          else if (height <= array(x + 1)(y)) 1
          else visibilityFor(x + 1, y, height, Direction.Down) + 1
        }
        case Direction.Left
        => if (y == 0) 0 else if (height <= array(x)(y - 1)) 1 else visibilityFor(x, y - 1, height, Direction.Left) + 1
        case Direction.Right
        => if (y == array(0).length - 1) 0 else if (height <= array(x)(y + 1)) 1 else visibilityFor(x, y + 1, height, Direction.Right) + 1
      }
    }

    array.indices.drop(1).dropRight(1).map(x => array(0).indices.drop(1).dropRight(1).map(y => {
      val up = visibilityFor(x, y, array(x)(y), Direction.Up)
      val down = visibilityFor(x, y, array(x)(y), Direction.Down)
      val left = visibilityFor(x, y, array(x)(y), Direction.Left)
      val right = visibilityFor(x, y, array(x)(y), Direction.Right)
      up *
        down *
        left *
        right
    }
    ).toArray
    ).toArray
  }


  def solve(input: Array[Array[Int]]): Int = {
    val array = Iterator(convertToVisibility(input), convertToVisibility(input.transpose).transpose).reduce(merge)
    array.map(a => a.count(b => b)).sum
  }

  def merge(a: Array[Array[Boolean]], b: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    a.zip(b).map { case (a, b) => a.zip(b).map { case (a, b) => a | b } }
  }

  def convertToVisibility(input: Array[Array[Int]]): Array[Array[Boolean]] = input.map(convertToVisibility)

  def convertToVisibility(input: Array[Int]): Array[Boolean] = {
    @tailrec
    def inner(input: Array[Int], lPointer: Int = 0, rPointer: Int = input.length - 1, lMax: Int = -1, rMax: Int = -1, result: Array[Boolean] = new Array[Boolean](input.length)): Array[Boolean] = {
      if (lPointer > rPointer) result
      else {
        val l = input(lPointer)
        val r = input(rPointer)
        var newLMax = lMax
        var newRMax = rMax
        var newLPointer = lPointer
        var newRPointer = rPointer
        var changed = false
        if (l > lMax) {
          result(lPointer) = true
          newLMax = l
          newLPointer = lPointer + 1
          changed = true
        }
        if (r > rMax) {
          result(rPointer) = true
          newRMax = r
          newRPointer = rPointer - 1
          changed = true
        }
        if (changed) inner(input, newLPointer, newRPointer, newLMax, newRMax, result)
        else {
          if (newLMax > newRMax) inner(input, lPointer, rPointer - 1, newLMax, newRMax, result)
          else inner(input, lPointer + 1, rPointer, newLMax, newRMax, result)
        }
      }
    }

    inner(input)
  }

}
