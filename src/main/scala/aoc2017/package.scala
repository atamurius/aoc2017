import java.nio.file.Path

import scala.io.Source
import scala.util.{Failure, Success, Try}

package object aoc2017 {

  class NotImplemented extends Exception("Not implemented")

  private def esc(modes: Int*)(content: String) =
    "\u001b[" + (modes mkString ";") +"m" + content + "\u001b[0m"


  trait Puzzle {

    type Input
    type Output

    val input: Input

    protected def linesOf(name: String): Iterator[String] =
      Source.fromFile(s"src/main/scala/aoc2017/$name").getLines()

    lazy val part1Answer: Output = throw new NotImplemented
    def part1(input: Input): Output = throw new NotImplemented
    lazy val part2Answer: Output = throw new NotImplemented
    def part2(input: Input): Output = throw new NotImplemented

    def main(args: Array[String]): Unit = {
      printResult("Part 1", part1(input), part1Answer)
      printResult("Part 2", part2(input), part2Answer)
    }

    private def printResult(part: String, actual: => Any, expected: => Any): Unit = {
      val red = esc(1, 31) _
      val darkRed = esc(31) _
      val green = esc(32) _
      val yellow = esc(33) _
      val gray = esc(37) _
      (Try(actual), Try(expected)) match {
        case (Failure(e), _) =>
          if (! e.isInstanceOf[NotImplemented]) {
            println(red(s"[FAILURE] $part: ${e.getMessage}"))
            e.printStackTrace()
          } else {
            println(gray(s"[FAILURE] $part: ${e.getMessage}"))
          }
        case (Success(res), Failure(_ : NotImplemented)) =>
          println(yellow(s"[RESULT] $part: $res"))
        case (Success(res), Success(exp)) if res != exp =>
          println(red(s"[FAILURE] $part: Unexpected result:)"))
          println(darkRed(s"\tActual  : $res\n\tExpected: $exp"))
        case (Success(res), _) =>
          println(green(s"[SUCCESS] $part: $res"))
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
}
