package aoc2017

import scala.annotation.tailrec

object Day20 extends Puzzle {

  type Num = Int

  implicit def toNum(str: String): Num = Integer.parseInt(str)

  case class Vec(x: Num = 0, y: Num = 0, z: Num = 0) {
    override def toString: String = s"<$x,$y,$z>"

    def + (v: Vec): Vec = Vec(
      x = x + v.x,
      y = y + v.y,
      z = z + v.z
    )

    def components: Iterator[Num] = productIterator.asInstanceOf[Iterator[Num]]

    def distanceFromCenter: Num = components.map(math.abs).sum

    def distanceTo(v: Vec): Num = (components zip v.components).map { case (a, b) => math.abs(a - b) }.sum
  }

  case class Point(position: Vec,
                   velocity: Vec,
                   acceleration: Vec) {
    override def toString: String =
      s"p=$position, v=$velocity, a=$acceleration ${if (isLeaving) "LEAVING" else if (isStable) "STABLE" else ""}"

    def next: Point = {
      val v = velocity + acceleration
      copy(velocity = v, position = position + v)
    }

    def isStable: Boolean = (velocity.productIterator zip acceleration.productIterator)
      .asInstanceOf[Iterator[(Num, Num)]]
      .forall {
        case (v, a) => a*v > 0 || a == 0
      }

    def isMovingOutside: Boolean = (position.productIterator zip velocity.productIterator)
      .asInstanceOf[Iterator[(Num, Num)]]
      .forall {
        case (p, v) => p*v > 0 || v == 0
      }

    def isLeaving: Boolean = isStable && isMovingOutside

    def <~> (p: Point): Num = position distanceTo p.position
  }

  object Point {
    private val Pattern = """(\w)=<(-?\d+),(-?\d+),(-?\d+)>""".r

    def parse(line: String): Point = {
      val parts = Pattern.findAllIn(line).map {
        case Pattern(t, x, y, z) => t -> Vec(x, y, z)
      }.toMap
      def part(t: String) = parts.getOrElse(t, sys.error(s"`$t` part not found in $line"))
      Point(
        position = part("p"),
        velocity = part("v"),
        acceleration = part("a")
      )
    }
  }

  override type Input = Seq[Point]

  override val input: Seq[Point] = linesOf("Day20.input").map(Point.parse).toSeq

  private val test = Seq(
    Point(Vec(3), Vec(2), Vec(-1)),
    Point(Vec(4), Vec(0), Vec(-2))
  )

  private def run(ps: Seq[Point]) = Iterator.iterate(ps) { _.map(_.next) }.takeWhile { _.exists(! _.isStable) }

  private def findNearest(ps: Seq[Point]) = {
    @tailrec
    def stabilize(seq: Iterator[Seq[Point]], prev: Seq[Point] = null): Seq[Point] =
      if (seq.hasNext) {
        val curr = seq.next()
        log("-------------------")
        curr.foreach { log(_) }
        stabilize(seq, curr)
      }
      else prev.map(_.next)

    val last = stabilize(run(ps))
    last.foreach { p => log(p) }
    val nearest = last.zipWithIndex.minBy { _._1.acceleration.distanceFromCenter }._2
    val p = ps(nearest).acceleration.distanceFromCenter
    val slow = last.zipWithIndex.filter(_._1.acceleration.distanceFromCenter == p)
    log("===================")
    log(s"Slowest:")
    slow.foreach { case (p, i) => log(s"$i: $p") }
    slow.minBy(_._1.velocity.distanceFromCenter)._2
  }

//  logged {
    findNearest(test) === 0
//  }

  override def part1(input: Seq[Point]): Any = findNearest(input)

  private def clearCollisions(ps: Seq[Point]) = ps.groupBy(_.position).collect { case (_, Seq(p)) => p }.toSeq

  @tailrec
  private def stabilizeWithoutCollisions(ps: Seq[Point]): Seq[Point] = {
    log("---------------")
    val current = clearCollisions(ps)
    current.foreach(log(_))
    val next = current.map(_.next)
    val dists = for {
      (prev1, nx1) <- current.iterator zip next.iterator
      (prev2, nx2) <- current.iterator zip next.iterator
    } yield (prev1 <~> prev2) <= (nx1 <~> nx2)
    if (current.exists(! _.isStable) || (dists contains false)) { // not all distances are growing
      stabilizeWithoutCollisions(next)
    } else {
      clearCollisions(next)
    }
  }

//  logged {
    stabilizeWithoutCollisions(Seq(
      Point(Vec(-6), Vec( 3), Vec(0)),
      Point(Vec(-4), Vec( 2), Vec(0)),
      Point(Vec(-2), Vec( 1), Vec(0)),
      Point(Vec( 3), Vec(-1), Vec(0))
    )).size === 1
//  }

  override def part2(input: Seq[Point]): Any = stabilizeWithoutCollisions(input).size
}
