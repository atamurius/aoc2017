package aoc2017

object Day14 extends Puzzle {

  override type Input = String

  override val input: String = "stpzcrnm"

  import Day10.knotHash

  def ones(i8: Int): Seq[Int] = (7 to 0 by -1).map { i => (i8 & (1 << i)) >> i }

  ones(0x00) === Seq(0, 0, 0, 0, 0, 0, 0, 0)
  ones(0x01) === Seq(0, 0, 0, 0, 0, 0, 0, 1)
  ones(0x0e) === Seq(0, 0, 0, 0, 1, 1, 1, 0)
  ones(0x0f) === Seq(0, 0, 0, 0, 1, 1, 1, 1)
  ones(0xff) === Seq(1, 1, 1, 1, 1, 1, 1, 1)

  private def prefix(str: String, size: Int = 1) = knotHash(str).take(size).flatMap(ones)
  private def bit(n: Int) = if (n == 0) '.' else '#'

  private val testGrid =
    """
      |##.#.#..
      |.#.#.#.#
      |....#.#.
      |#.#.##.#
      |.##.#...
      |##..#..#
      |.#...#..
      |##.#.##.
    """.trim.stripMargin.lines.map {
      _.toCharArray.map { c =>
        if (c == '#') 1 else 0
      }.toArray
    }.toArray

  (0 to 7).foreach { i =>
    prefix(s"flqrgnkx-$i") === testGrid(i).toSeq
  }

  private def grid(str: String) = (0 until 128).map { i => knotHash(s"$str-$i").iterator.flatMap(ones) }

  private def onesInGrid(str: String) = grid(str).foldLeft(0) { _ + _.sum }

  onesInGrid("flqrgnkx") === 8108

  override def part1(input: String): Any = onesInGrid(input)

  private def materialGrid(str: String) = grid(str).map { _.toArray }.toArray

  private def near(grid: Array[Array[Int]], pos: (Int, Int)) = for {
    (dx, dy) <- Seq((0,1),(0,-1),(1,0),(-1,0))
    (oldX, oldY) = pos
    x = oldX + dx
    y = oldY + dy
    if x >= 0 && x < grid(0).length && y >= 0 && y < grid.length
  } yield grid(y)(x)

  private def paint(grid: Array[Array[Int]]): Seq[Int] = {
    object Colors {
      private var fakeColors = Map[Int, Int]() withDefault identity // fake color -> true color
      def isFake(c: Int): Boolean = fakeColors contains c
      def trueColor(c: Int): Int = fakeColors(c)

      private var next = 2
      def nextColor: Int = try next finally next += 1

      def makeFake(fake: Int, trueC: Int): Unit = {
        fakeColors += fake -> trueC
        fakeColors.foreach {
          case (c, maybeTrue) =>
            if (maybeTrue == fake) {
              fakeColors += c -> trueC
            }
        }
        log(s"$fake is fake, $trueC is true: $fakeColors")
      }
      def trueColors: Seq[Int] = (2 until next).filterNot(isFake).sorted
    }
    val Empty = 0
    val Unknown = 1

    for { // each cell of unknown color
      y <- grid.indices
      x <- grid(y).indices
      current = grid(y)(x)
      if current == Unknown
    } {
      val adjacents = near(grid, (x, y))
      val newColor =
        adjacents.find { _ > Unknown }.map(Colors.trueColor) getOrElse
          Colors.nextColor

      grid(y)(x) = newColor

      // mark all adjucent colors as fake
      adjacents.foreach { color =>
        if (color > Unknown && color != newColor)
          Colors.makeFake(color, newColor)
      }
    }
    // repaint to true colors
    for {
      y <- grid.indices
      x <- grid(y).indices
      current = grid(y)(x)
      if Colors.isFake(current)
    } grid(y)(x) = Colors.trueColor(current)

    val res = Colors.trueColors
    log(s"true colors = $res")
    res
  }

  paint(testGrid).size === 12
//    testGrid.foreach { row =>
//      log( row.map { "%3d".format(_) }.mkString)
//    }

  override def part2(input: String): Any = paint(materialGrid(input)).size
}
