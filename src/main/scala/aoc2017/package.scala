import scala.io.Source
import scala.util.{Failure, Success, Try}

package object aoc2017 {

  class NotImplemented extends Exception("Not implemented")

  private def esc(modes: Int*)(content: String) =
    "\u001b[" + (modes mkString ";") +"m" + content + "\u001b[0m"


  trait Puzzle {

    type Input

    val input: Input

    private var testSucceed = true

    private var logging = false

    protected def linesOf(name: String): Iterator[String] =
      Source.fromFile(s"src/main/scala/aoc2017/$name").getLines()

    def part1(input: Input): Any = throw new NotImplemented
    def part2(input: Input): Any = throw new NotImplemented

    def main(args: Array[String]): Unit = {
      if (testSucceed) {
        timed {
          printResult("Part 1", part1(input))
        }
        timed {
          printResult("Part 2", part2(input))
        }
      }
    }

    protected def timed[T](f: => T): T = {
      val start = System.currentTimeMillis
      val result = f
      val time = System.currentTimeMillis - start
      if (time > 100) println(esc(37)("[TIME] %.3f%s".format(
        if (time > 1000) time / 1000d else time.toDouble,
        if (time > 1000) "s" else "ms"
      )))
      result
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

    implicit def expectation(value: Any): Expectation = new Expectation(value)(testSucceed = false)

    def logged[T](value: => T): T = try {
      logging = true
      value
    } finally {
      logging = false
    }

    def log(msg: => Any): Unit = if (logging) println("[LOG] " + msg)
  }

  class Expectation(val value: Any)(onFailure: => Any) {
    def === (other: Any): Unit = {
      if (value != other) {
        println(esc(31)(s"[TEST FAILED] $value != $other"))
        onFailure
      }
    }
  }
}
