package aoc2017

object Day22 extends Puzzle {

  override type Input = Seq[String]

  override val input: Seq[String] = linesOf("Day22.input").toSeq

  trait State {
    val empty: Char
    def next(c: Char): Char
  }

  case class Nodes(state: State,
                   private var statuses: Map[(Int, Int), Char] = Map.empty) {

    def apply(pos: (Int, Int)): Char = statuses.getOrElse(pos, state.empty)

    def mark(pos: (Int, Int), c: Char): Unit =
      if (c == state.empty) statuses -= pos
      else statuses += pos -> c

    def mark(pos: (Int, Int)): Char = {
      val next = state.next(this(pos))
      mark(pos, next)
      next
    }

    def draw(mark: ((Int, Int), Char, Char) = ((0, 0), ' ', ' ')): Seq[String] = {
      val ((mx, my), ml, mr) = mark
      val area = statuses.keySet + mark._1
      val minX = area.minBy(_._1)._1
      val minY = area.minBy(_._2)._2
      val maxX = area.maxBy(_._1)._1
      val maxY = area.maxBy(_._2)._2

      (minY to maxY).map { y =>
        (minX to maxX).map { x =>
          val current = this(x, y)
          val (left, right) = if (x == mx && y == my) (ml, mr) else (' ', ' ')
          s"$left$current$right"
        }.mkString
      }
    }
  }

  object Dir extends Enumeration {
    val Up, Down, Left, Right = Value
    type Dir = Value
    val clockwise = Map(
      Up -> Right,
      Right -> Down,
      Down -> Left,
      Left -> Up
    )
    val counterClockwise: Map[Dir, Dir] = clockwise.map { case (a, b) => (b, a) }
  }
  import Dir._

  trait Strategy {
    def turn(state: Char, dir: Dir): Dir
    val infected: Char
  }

  case class Virus(strategy: Strategy, nodes: Nodes) {

    def this(rules: Strategy with State) = this(rules, Nodes(rules))

    private var pos = (0, 0)
    private var dir = Up
    private var infectedCount = 0

    def infected: Int = infectedCount

    def position: (Int, Int) = pos

    def draw: Seq[String] = {
      nodes.draw(dir match {
        case Up => (position, '/', '\\')
        case Down => (position, '\\', '/')
        case Left => (position, '<', ']')
        case Right => (position, '[', '>')
      })
    }

    def configure(lines: Seq[String]): this.type = {
      for {
        (line, y) <- lines.zipWithIndex
        (curr, x) <- line.zipWithIndex
      } nodes.mark((x, y), curr)
      pos = (lines.size / 2, lines.head.length / 2)
      this
    }

    def burst(): this.type = {
      dir = strategy.turn(nodes(position), dir)

      if (nodes.mark(position) == strategy.infected)
        infectedCount += 1

      pos = (pos, dir) match {
        case ((x, y), Up) => (x, y - 1)
        case ((x, y), Right) => (x + 1, y)
        case ((x, y), Down) => (x, y + 1)
        case ((x, y), Left) => (x - 1, y)
      }

      this
    }
  }

  val initialRules: State with Strategy = new State with Strategy {
    override val empty: Char = '.'

    override def next(c: Char): Char = if (c == empty) infected else empty

    override def turn(state: Char, dir: Dir): Dir =
      if (state == infected) clockwise(dir) else counterClockwise(dir)

    override val infected: Char = '#'
  }

  private val testMap =
    """
      |..#
      |#..
      |...
    """.stripMargin.trim.lines.toSeq

  { // test
    val v = new Virus(initialRules).configure(testMap)
    v.draw.foreach(log(_))
    (1 to 7).foreach { i =>
      v.burst()
      log(s"--- $i -------------------")
      v.draw.foreach(log(_))
    }
    v.infected === 5
    (70 - 7) times { v.burst() }
    log("--- 70 -------------------")
    v.draw.foreach(log(_))
    v.infected === 41

    (10000 - 70) times{ v.burst() }
    v.infected === 5587
  }

  override def part1(input: Seq[String]): Any = {
    val virus = new Virus(initialRules).configure(input)
    10000 times { virus.burst() }
    virus.infected
  }

  private val updatedRules = new State with Strategy {
    override val empty: Char = '.'
    val weakened = 'W'
    val flagged = 'F'
    override val infected: Char = '#'

    override def next(c: Char): Char = c match {
      case `empty` => weakened
      case `weakened` => infected
      case `infected` => flagged
      case `flagged` => empty
    }

    override def turn(state: Char, dir: Dir): Dir = state match {
      case `empty` => counterClockwise(dir)
      case `weakened` => dir
      case `infected` => clockwise(dir)
      case `flagged` => (clockwise andThen clockwise)(dir)
    }
  }

  { // test
    val v = new Virus(updatedRules).configure(testMap)
    7 times {
      v.draw.foreach(log(_))
      log("---------------------")
      v.burst()
    }
    v.draw.foreach(log(_))
    v.infected === 1
    (100 - 7) times { v.burst() }
    v.infected === 26
//    (10*1000*1000 - 100) times { v.burst() }
//    v.infected === 2511944
  }

  override def part2(input: Seq[String]): Any =
    new Virus(updatedRules)
      .configure(input)
      .times(10*1000*1000) { _.burst() }
      .infected
}
