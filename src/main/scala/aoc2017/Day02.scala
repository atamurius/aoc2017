package aoc2017

object Day02 extends Puzzle {

  override type Input = Seq[Seq[Int]]
  override type Output = Int

  override val input = linesOf("Day02.input").map { _.split("\\s+").map { _.toInt }.toSeq }.toSeq

  /**
   * For each row, determine the difference between the largest value and the smallest value;
   * the checksum is the sum of all of these differences.
   */
  override def part1(input: Seq[Seq[Int]]): Int = input.map { row => row.max - row.min }.sum

  override lazy val part1Answer = 42378

  /**
   * It sounds like the goal is to find the only two numbers in each row where one evenly divides
   * the other - that is, where the result of the division operation is a whole number.
   * They would like you to find those numbers on each line, divide them, and add up each line's result.
   */
  override def part2(input: Seq[Seq[Int]]): Int = input.map(part2RowValue).sum

  private def part2RowValue(row: Seq[Int]) = {
    val result = for {
      (x, i) <- row.zipWithIndex
      y <- row drop (i + 1)
      if x % y == 0 || y % x == 0
    } yield if (x > y) x / y else y / x
    result.head
  }

  part2RowValue(Seq(5, 9, 2, 8)) === 4
  part2RowValue(Seq(9, 4, 7, 3)) === 3
  part2RowValue(Seq(3, 8, 6, 5)) === 2

  override lazy val part2Answer = 246
}
