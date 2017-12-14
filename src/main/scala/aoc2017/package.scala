import java.nio.file.Path

import scala.io.Source
import scala.util.{Failure, Success, Try}

package object aoc2017 {

  class NotImplemented extends Exception("Not implemented")

  private def esc(modes: Int*)(content: String) =
    "\u001b[" + (modes mkString ";") +"m" + content + "\u001b[0m"


  trait Puzzle {

    type Input

    val input: Input

    protected def linesOf(name: String): Iterator[String] =
      Source.fromFile(s"src/main/scala/aoc2017/$name").getLines()

    def part1(input: Input): Any = throw new NotImplemented
    def part2(input: Input): Any = throw new NotImplemented

    def main(args: Array[String]): Unit = {
      printResult("Part 1", part1(input))
      printResult("Part 2", part2(input))
    }

    private def printResult(part: String, actual: => Any): Unit = {
      val red = esc(1, 31) _
      val darkRed = esc(31) _
      val green = esc(32) _
      val yellow = esc(33) _
      val gray = esc(37) _
      Try(actual) match {
        case Failure(e) =>
          if (! e.isInstanceOf[NotImplemented]) {
            println(red(s"[FAILURE] $part: ${e.getMessage}"))
            e.printStackTrace()
          } else {
            println(gray(s"[FAILURE] $part: ${e.getMessage}"))
          }
        case Success(res) =>
          println(yellow(s"[RESULT] $part: $res"))
      }
    }
  }

  implicit class Expectation(val value: Any) extends AnyVal {
    def === (other: Any): Unit = {
      if (value != other) {
        println(esc(31)(s"[TEST FAILED] $value != $other"))
      }
    }
  }

  private var logging = false

  def logged[T](value: => T): T = try {
    logging = true
    value
  } finally {
    logging = false
  }

  def log(msg: => String): Unit = if (logging) println("[LOG] " + msg)
}
