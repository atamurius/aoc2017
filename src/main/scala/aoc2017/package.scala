import scala.util.{Failure, Success, Try}

package object aoc2017 {

  class NotImplemented extends Exception("Not implemented")

  trait Puzzle {

    type Input
    type Output

    val input: Input

    lazy val part1Answer: Output = throw new NotImplemented
    def part1(input: Input): Output = throw new NotImplemented
    lazy val part2Answer: Output = throw new NotImplemented
    def part2(input: Input): Output = throw new NotImplemented

    def main(args: Array[String]): Unit = {
      printResult("Part 1", part1(input), part1Answer)
      printResult("Part 2", part2(input), part2Answer)
    }

    private def printResult(part: String, actual: => Any, expected: => Any): Unit = {
      (Try(actual), Try(expected)) match {
        case (Failure(e), _) =>
          println(s"[FAILURE] $part: ${e.getMessage}")
          if (! e.isInstanceOf[NotImplemented]) {
            e.printStackTrace()
          }
        case (Success(res), Failure(_ : NotImplemented)) =>
          println(s"[RESULT] $part: $res")
        case (Success(res), Success(exp)) if res != exp =>
          println(s"[FAILURE] $part: Unexpected result:\n$res\nExpected: $exp")
        case (Success(res), _) =>
          println(s"[SUCCESS] $part: $res")
      }
    }
  }
}
