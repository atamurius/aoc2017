package aoc2017

object Day13 extends Puzzle {

  override type Input = Seq[Gate]

  case class Gate(depth: Int, range: Int) {
    def hitAt(time: Int): Boolean = time % (2 * range - 2) == 0
  }

  override val input: Input = linesOf("Day13.input").map(_ split ": ").map {
    case Array(depth, range) => Gate(depth.toInt, range.toInt)
  }.toSeq

  def hits(gates: Seq[Gate]): Seq[Gate] = gates.filter { g => g.hitAt(g.depth) }

  val testData = Seq(
    Gate(0, 3),
    Gate(1, 2),
    Gate(4, 4),
    Gate(6, 4)
  )

  hits(testData).map(_.depth) === Seq(0, 6)

  def severity(p: Seq[Gate]): Int = p.foldLeft(0) { (a, g) => a + g.depth * g.range }

  severity(hits(testData)) === 24

  override def part1(input: Seq[Gate]): Any = severity(hits(input))

  def passThrough(gates: Seq[Gate]): Int = {
    var offset = 0
    while (true) {
      if (gates.forall { g => ! g.hitAt(g.depth + offset) })
        return offset
      offset += 1
    }
    -1
  }

  passThrough(testData) === 10

  override def part2(input: Seq[Gate]): Any = passThrough(input)
}
