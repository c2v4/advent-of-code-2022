package com.c2v4.advent22

import scala.collection.{immutable, mutable}
import scala.io.Source
import scala.util.Using
import scala.collection.mutable.Map

// A File is either a directory or a file with a size
sealed trait File:
  val parent: Dir
  val name: String

case class Dir(parent: Dir, name: String, children: mutable.Map[String, File] = mutable.Map.empty) extends File

case class FileWithSize(parent: Dir, name: String, size: Long) extends File

// An Operation is a pair of input and output
type Operation = (String, List[String])

// Parse the input and create a filesystem
def parseInput(input: List[String]): Dir = {
  // The filesystem is initially empty
  val filesystem = Dir(null, "/", mutable.Map.empty)

  // The current directory is initially the outermost directory
  var currentDir = filesystem

  val operations = getOperations(input)

  // Loop through all the lines of the input
  for (operation <- operations) {
    // Split the line into the command and the arguments
    val parts = operation._1.split(" ")
    val command = parts(0)
    val args = parts.tail


    command match {
      // If the command is "cd", change the current directory
      case "cd" =>
        args(0) match {
          // If the argument is "..", move up one level
          case ".." =>
            currentDir = currentDir.parent
          // If the argument is "/", move to the outermost directory
          case "/" =>
            currentDir = filesystem
          // Otherwise, move down one level to the specified directory
          case _ =>
            currentDir = currentDir.children(args(0)).asInstanceOf[Dir]
        }

      // If the command is "ls", print the contents of the current directory
      case "ls" =>
        for (line <- operation._2) {
          val parts = line.split(" ")
          parts(0) match {
            case "dir" => currentDir.children.getOrElseUpdate(parts(1), Dir(currentDir, parts(1)))
            case _ => currentDir.children.getOrElseUpdate(parts(1), FileWithSize(currentDir, parts(1), parts(0).toLong))
          }
        }

    }
  }

  // Return the filesystem
  filesystem
}

def getOperations(input: List[String]): List[Operation] = {
  input.foldLeft(List.empty[Operation])((acc, line) => {
    if (line.startsWith("$")) acc :+ (line.drop(2), List.empty)
    else {
      acc.dropRight(1) :+ (acc.last._1, acc.last._2 :+ line)
    }
  })
}

// Calculate the total size of a directory
def totalSize(dir: Dir, results: immutable.Map[String, Long] = immutable.Map.empty, pwd: String = "/"): immutable.Map[String, Long] = {
  // The total size is initially 0
  var sum = 0L

  var tempRes = results
  // Add the size of each file in the directory
  for ((name, file) <- dir.children) {
    file match {
      case Dir(_, _, _) => {
        val key = pwd + name + "/"
        if(!tempRes.contains(key)) {
          tempRes = totalSize(file.asInstanceOf[Dir], tempRes, key)
        }
        sum += tempRes(key)
      }
      case FileWithSize(_, _, size) => sum += size
    }
  }

  // Return the total size
  tempRes + (pwd -> sum)
}

// Find all of the directories with a total size of at most 100000
// and return the sum of their total sizes
def findDirectories(filesystem: Dir): Long = {
  // The sum of the total sizes is initially 0
  var sum = 0L
  // Loop through all the files in the outermost directory
  for ((name, size) <- totalSize(filesystem)) {
    // If the file is a directory and its total size is at most 100000,
    // add its total size to the sum
    if (size <= 100000) sum += size
  }

  // Return the sum of the total sizes
  sum
}

def smallestToDelete(filesystem: Dir): Long = {
  val totalSizes = totalSize(filesystem)
  val sorted = totalSizes.toList.map(_._2).sorted
  sorted.filter(_ > 30000000-(70000000L - totalSizes("/"))).head
  }


@main def main7: Unit = {
  // Find all of the directories with a total size of at most 100000
  // and print the sum of their total sizesUsing(Source.fromFile("6.txt")) {
  Using(Source.fromFile("7.txt")) {
    input =>
      val dir = parseInput(input.getLines().toList)
      println(findDirectories(dir))
      println(smallestToDelete(dir))
  }

}