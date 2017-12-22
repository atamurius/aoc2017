package aoc2017

object Day19 extends Puzzle {

  override type Input = Seq[String]

  override val input: Seq[String] = linesOf("Day19.input").toSeq

  private val test =
    """
      |     |
      |     |  +--+
      |     A  |  C
      | F---|----E|--+
      |     |  |  |  D
      |     +B-+  +--+
      |
    """.trim.stripMargin.lines.toSeq

  object Pipes {
    type Dir = (Int, Int)
    case class Pos(x: Int, y: Int) {
      def + (d: Dir): Pos = Pos(x + d._1, y + d._2)
    }
    val directions: Set[Dir] = Set((0, 1), (1, 0), (0, -1), (-1, 0))
    def opposite(dir: Dir): Dir = (-dir._1, -dir._2)
  }

  case class Pipes(lines: Seq[String]) {
    import Pipes._
    var pos: Pos = Pos(lines.head indexOf '|', 0)
    var dir: Dir = (0, 1)
    var letters = Seq.empty[Char]

    def apply(p: Pos): Char = {
      val line = if (lines.indices contains p.y) Some(lines(p.y)) else None
      line.filter { _.indices contains p.x }.map { _(p.x) } getOrElse ' '
    }
    def at(p: Pos): Char = apply(p)

    def debug(): Unit = {
      log(s"\n--- ${letters.mkString}")
      lines.zipWithIndex.foreach { case (row, y) =>
        if (pos.y != y) log(row)
        else log(row updated (pos.x, '*'))
      }
    }
    def step(): Boolean = {
      pos = pos + dir
      at(pos) match {
        case c if c.isLetter =>
          letters = letters :+ c
        case '+' =>
          dir = (directions - opposite(dir)).find { d => at(pos + d) != ' ' } getOrElse
            sys.error(s"Don't know where to go from $pos")
        case _ => ()
      }
      at(pos) != ' '
    }

    def walkThrough(): (String, Int) = {
      var steps = 1
      while (step()) steps += 1
      (letters.mkString, steps)
    }
  }

  Pipes(test).walkThrough() === ("ABCDEF", 38)

  override def part1(input: Seq[String]): Any = Pipes(input).walkThrough()._1

  override def part2(input: Seq[String]): Any = Pipes(input).walkThrough()._2
}
