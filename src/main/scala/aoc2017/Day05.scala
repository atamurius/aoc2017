package aoc2017

object Day05 extends Puzzle {

  override type Input = Seq[Int]
  override type Output = Int

  override val input: Seq[Int] = linesOf("Day05.input").map(_.toInt).toSeq

  override def part1(input: Seq[Int]): Int = countToExit(new Program(input))

  override lazy val part1Answer: Int = 388611

  private class Program(initial: Seq[Int], mutate: Int => Int = _ + 1) {
    private val jumps = initial.to[collection.mutable.ArrayBuffer]
    private var position = 0

    def jump(): Boolean = if (jumps.indices contains position) {
      val next = position + jumps(position)
      jumps(position) = mutate(jumps(position))
      position = next
      true
    } else false

    override def toString: String = jumps.zipWithIndex.map {
      case (jump, i) if i == position => s"($jump)"
      case (jump, _) => jump.toString
    }.mkString(" ")
  }

  private val testSeq = Seq(0, 3, 0, 1, -3)
  private val test = new Program(testSeq)
  test.toString === "(0) 3 0 1 -3"
  test.jump() === true
  test.toString === "(1) 3 0 1 -3"
  test.jump()
  test.jump()
  test.toString === "2 4 0 1 (-3)"

  private def countToExit(p: Program, cnt: Int = 0): Int =
    if (p.jump()) countToExit(p, cnt + 1)
    else cnt

  countToExit(new Program(testSeq)) === 5

  override def part2(input: Seq[Int]): Int = countToExit(new Program(input, decThree))

  override lazy val part2Answer: Int = 27763113

  private def decThree(n: Int) = if (n >= 3) n - 1 else n + 1

  countToExit(new Program(testSeq, decThree)) === 10
}
