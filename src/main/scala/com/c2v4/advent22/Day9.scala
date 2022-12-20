package com.c2v4.advent22

import com.c2v4.advent22.Day9.Direction.Direction

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day9 {

  type Position = (Int, Int)

  case class State(rope: List[Position], tailVisited: Set[Position] = Set((0, 0)))


  object Direction {

    sealed trait Direction {

      def move(position: Position): Position = this match {
        case Up => (position._1, position._2 + 1)
        case Down => (position._1, position._2 - 1)
        case Left => (position._1 - 1, position._2)
        case Right => (position._1 + 1, position._2)
      }
    }

    case object Up extends Direction

    case object Down extends Direction

    case object Left extends Direction

    case object Right extends Direction

    def fromString(input: String): Direction = input match {
      case "U" => Up
      case "D" => Down
      case "L" => Left
      case "R" => Right
    }
  }

  case class Move(direction: Direction, steps: Int)

  object Move {
    def fromString(input: String): Move = {
      val strings = input.split(" ")
      val direction = Direction.fromString(strings.head)
      val steps = strings.last.toInt
      Move(direction, steps)
    }
  }

  def main(args: Array[String]): Unit = {
    Using(Source.fromFile("9.txt")) {
      input =>
        val moves = input.getLines().map(Move.fromString)
        val size1 = moves
          .foldLeft(State(List((0, 0), (0, 0))))((state, move) => stateReducer(state, move)).tailVisited.size
        val size2 = moves
          .foldLeft(State(List.fill(10)(0,0)))((state, move) => stateReducer(state, move)).tailVisited.size
        println(size1)
        println(size2)
    }
  }

  def stateReducer(state: State, move: Move) = {
    Range.inclusive(1, move.steps).foldLeft(state)((s, _) => {
      val newHead = move.direction.move(s.rope.head)
      val newRope = s.rope.tail.foldLeft(List(newHead))((acc, position) => {
        acc :+ calculateTailPosition(position, acc.last)
      })
      val newTailVisited = s.tailVisited + newRope.last
      State(newRope, newTailVisited)
    })
  }

  def calculateTailPosition(currentTail: (Int, Int), newHead: (Int, Int)): Position = {
    if(currentTail == newHead) return currentTail
    val (x1, y1) = currentTail
    val (x2, y2) = newHead
    manhattanDistance(currentTail, newHead) match
      case 0 | 1 => currentTail
      case 2 => if(x1 != x2 && y1 != y2) currentTail else (x1 + math.signum(x2 - x1), y1 + math.signum(y2 - y1))
      case _ => (x1 + math.signum(x2 - x1), y1 + math.signum(y2 - y1))

  }

  def manhattanDistance(first: Position, second: Position): Int = {
    val (x1, y1) = first
    val (x2, y2) = second
    Math.abs(x1 - x2) + Math.abs(y1 - y2)
  }


}
