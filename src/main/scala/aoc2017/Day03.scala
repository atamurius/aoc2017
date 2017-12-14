package aoc2017

import scala.annotation.tailrec
import scala.util.Try

object Day03 extends Puzzle {

  override type Input = Int

  override val input = 361527

  /**
   * Each square on the grid is allocated in a spiral pattern.
   * Find the Manhattan Distance between the location of the data and square 1.
   */
  override def part1(input: Int): Any = manhattan(input)

  private def squareSide(n: Int, q: Int = 1): Int =
    if (q*q >= n) q
    else squareSide(n, q + 2)

  squareSide(12) === 5

  private def positionOf(n: Int) = {
    val side = squareSide(n) - 1
    val inner = (side - 1) * (side - 1)
    val offset = n - inner
    val delta = side/2
    log(s"side: $side, inner: $inner, offset: $offset")
    if (offset <= side) (side - delta, offset - delta)
    else if (offset <= side*2) (side - offset + delta, side - delta)
    else if (offset <= side*3) (delta - side, delta - offset + side*2)
    else (offset - side*3 - delta, delta - side)
  }

  positionOf(11) === ( 2,  0)
  positionOf(13) === ( 2,  2)
  positionOf(15) === ( 0,  2)
  positionOf(17) === (-2,  2)
  positionOf(19) === (-2,  0)
  positionOf(21) === (-2, -2)
  positionOf(23) === ( 0, -2)
  positionOf(25) === ( 2, -2)

  private def manhattan(n: Int) = if (n == 1) 0 else {
    val (dx, dy) = positionOf(n)
    log(s"$n -> ($dx, $dy)")
    Math.abs(dx) + Math.abs(dy)
  }

  manhattan(1) === 0
  manhattan(12) === 3
  manhattan(16) === 3
  manhattan(23) === 2
  manhattan(1024) === 31

  /**
   *  As a stress test on the system, the programs here clear the grid and then store the value 1 in square 1.
   *  Then, in the same allocation order as shown above,
   *  they store the sum of the values in all adjacent squares, including diagonals.
   *  What is the first value written that is larger than your puzzle input?
   */
  override def part2(input: Int): Any = iterateUntil { _ > input }

  private class Cursor {
    var x: Int = 0
    var y: Int = 0
    var side = 1
    var phase = 3 // up, left, down, right
    var counter = 0

    def next(): (Int, Int) = {
      val result = (x, y)
      counter += 1
      if (counter >= side - 1) {
        phase += 1
        counter = 0
      }
      if (phase == 4) {
        phase = 0
        side += 2
        x += 1
      } else phase match {
        case 0 => y -= 1
        case 1 => x -= 1
        case 2 => y += 1
        case 3 => x += 1
      }
      result
    }
  }

  // for test
  private def draw(side: Int): Unit = {
    val cursor = new Cursor
    val data = Array.ofDim[Int](side, side)
    val delta = side / 2
    var i = 0
    while (i < side*side) {
      val (x, y) = cursor.next()
      i += 1
      data(y + delta)(x + delta) = i
    }
    data.foreach { row =>
      println(row.map { "%4d".format(_) }.mkString)
    }
  }

  private def neighbours(xy: (Int, Int)): Seq[(Int, Int)] =
    for {
      dx <- Seq(-1, 0, 1)
      dy <- Seq(-1, 0, 1)
      (x, y) = xy
    } yield (x + dx, y + dy)

  Set(neighbours((0, 0)) : _*) === Set((0, 0), (0, 1), (0, -1), (1, 0), (1, -1), (1, 1), (-1, 0), (-1, 1), (-1, -1))

  private class Cache extends (((Int, Int)) => Option[Int]) {
    var values: List[((Int, Int), Int)] = Nil
    var total = 0
    var next = 1
    def update(xy: (Int, Int), v: Int): Unit = {
      values = (xy -> v) :: values
      total += 1
      if (total == next*next) {
        val prev = (next - 2) max 0
        values = values.take(total - prev*prev)
        next += 2
      }
    }
    def apply(xy: (Int, Int)): Option[Int] = values.find(_._1 == xy).map(_._2)
  }

  private def iterate[T](f: ((Int, Int), Cache) => Option[T]): T = {
    val cursor = new Cursor
    val cache = new Cache
    @tailrec def call(): T = f(cursor.next(), cache) match {
      case Some(x) => x
      case None => call()
    }
    call()
  }

  private def iterateUntil(f: Int => Boolean) = iterate[Int] { (xy, cache) =>
    val ns = neighbours(xy).map(cache).collect {
      case Some(x) => x
    }
    val sum = ns.sum
    log(s"$xy -> $sum: $ns")
    cache(xy) = sum max 1
    Some(sum).filter(f)
  }

  iterateUntil { _ >= 806 } === 806
}
