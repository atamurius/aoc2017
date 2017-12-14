package aoc2017

import aoc2017.Day11.path

import scala.annotation.tailrec

object Day11 extends Puzzle {

  object Direction extends Enumeration {
    val North, NorthEast, SouthEast, South, SouthWest, NorthWest = Value
    type Direction = Value
  }
  import Direction._

  override type Input = Seq[Direction]

  override val input: Seq[Direction] = parse(linesOf("Day11.input").next())

  private def parse(dirs: String): Seq[Direction] =
    dirs.split("\\,").map {
      case "n"  => North
      case "s"  => South
      case "ne" => NorthEast
      case "se" => SouthEast
      case "sw" => SouthWest
      case "nw" => NorthWest
    }

  type Pos = (Int, Int)
  val origin: Pos = (0, 0)

  /*   . . .
   * . . * . x @ @
   *   . . x @ * @ @
   *         @ @ @
   */
  private def move(pos: Pos, dir: Direction): Pos = (pos, dir) match {
    case ((x, y), North)      => (x, y - 2)
    case ((x, y), NorthEast)  => (x + 2, y - 1)
    case ((x, y), NorthWest)  => (x - 2, y - 1)
    case ((x, y), South)      => (x, y + 2)
    case ((x, y), SouthEast)  => (x + 2, y + 1)
    case ((x, y), SouthWest)  => (x - 2, y + 1)
  }

  private def move(start: Pos = origin, path: Seq[Direction]): Pos =
    path.foldLeft(start)(move)

  move(path = parse("ne,ne,sw,sw")) === origin

  private def distance(a: Pos, b: Pos): Int = {
    val dx = a._1 - b._1
    val dy = a._2 - b._2
    dx*dx + dy*dy
  }

  private def closestDirection(from: Pos, to: Pos): Direction =
    Direction.values.minBy { dir =>
      distance(move(from, dir), to)
    }

  closestDirection(origin, move(path = parse("ne,ne,s,s"))) === SouthEast

  @tailrec
  private def path(from: Pos = origin, to: Pos, prev: Seq[Direction] = Seq.empty): Seq[Direction] =
    if (from == to) prev
    else {
      val dir = closestDirection(from, to)
      path(move(from, dir), to, prev :+ dir)
    }

  @tailrec
  private def pathLen(from: Pos = origin, to: Pos, count: Int = 0): Int =
    if (from == to) count
    else {
      val dir = closestDirection(from, to)
      pathLen(move(from, dir), to, count + 1)
    }

  path(to = move(path = parse("ne,ne,s,s"))) === Seq(SouthEast, SouthEast)
  path(to = move(path = parse("se,sw,se,sw,sw"))).size === 3
  path(to = move(path = parse("ne,ne,ne"))).size === 3
  path(to = move(path = parse("ne,ne,sw,sw"))).size === 0

  override def part1(input: Seq[Direction]): Any = path(to = move(path = input)).size

  override def part2(input: Seq[Direction]): Any = {
    val positions = input.scanLeft(origin)(move)
    var lastMax = (0, 0)
    positions.map { pos =>
      val dist = distance(pos, origin)
      if (dist * 1.1 > lastMax._2) {
        val res = pathLen(to = pos)
        if (lastMax._1 < res) {
          lastMax = (res, dist)
        }
        res
      } else 0
    }.max
  }

  private def draw(path: Seq[Direction], specials: Seq[(Pos, Char)] = Seq.empty): Unit = {
    val positions = path.scanLeft(origin)(move)
    val (minx, miny, maxx, maxy) = positions.foldLeft((0, 0, 0, 0)) { (prev, curr) =>
      (prev, curr) match {
        case ((ix, iy, ax, ay), (x, y)) => (
          ix min x,
          iy min y,
          ax max x,
          ay max y
        )
      }
    }
    val width = maxx - minx
    val height = maxy - miny
    log(s"$width x $height ($minx, $miny) x ($maxx, $maxy)")
    val canvas = Array.ofDim[Char](height + 1, width + 1)
    canvas.foreach { line => line.indices.foreach { i => line(i) = '.' } }
    positions.foreach {
      case (x, y) =>
        canvas(y - miny)(x - minx) = '@'
    }
    canvas(0 - miny)(0 - minx) = '*'
    specials.foreach {
      case ((x, y), c) => canvas(y - miny)(x - minx) = c
    }
    canvas.foreach { line =>
      println(line.mkString)
    }
  }
}
