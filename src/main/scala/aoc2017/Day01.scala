package aoc2017

object Day01 extends Puzzle {

  override type Input = Seq[Int]

  override val input: Seq[Int] = linesOf("Day01.input").next().map(_.toString.toInt)

  /**
   * The captcha requires you to review a sequence of digits (your puzzle input)
   * and find the sum of all digits that match the next digit in the list.
   * The list is circular, so the digit after the last digit is the first digit in the list.
   */
  override def part1(digits: Seq[Int]): Any = (digits :+ digits.head)
    .sliding(2)
    .collect { case Seq(x, y) if x == y => x }
    .sum

  /**
   * Now, instead of considering the next digit, it wants you to consider the digit
   * halfway around the circular list.
   */
  override def part2(input: Seq[Int]): Any = {
    def halfway(i: Int): Int = input((i + input.size / 2) % input.size)

    input.zipWithIndex.collect { case (x, i) if x == halfway(i) => x }.sum
  }
}
