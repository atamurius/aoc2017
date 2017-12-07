package aoc2017

object Day06 extends Puzzle {

  override type Input = Seq[Int]
  override type Output = Int

  override val input: Seq[Int] = Seq(0,	5,	10,	0,	11,	14,	13,	4,	11,	8,	8,	7,	1,	4,	12,	11)

  private def redistribute(banks: Seq[Int]): Seq[Int] = {
    val (amount, source) = banks.zipWithIndex.foldLeft(-1 -> -1) { (prev, curr) =>
      if (prev._1 < curr._1) curr
      else prev
    }
    log(s"$source ($amount) from $banks")
    if (amount == 0) banks
    else {
      val each = amount / banks.size
      val add = amount - each*banks.size
      log(s"each: $each, add: $add")
      banks.updated(source, 0).zipWithIndex.map {
        case (x, i) if (i - 1 + banks.size - source) % banks.size < add =>
          log(s"[$i] + 1")
          x + each + 1
        case (x, _) => x + each
      }
    }
  }

  redistribute(Seq(0, 2, 7, 0)) === Seq(2, 4, 1, 2)
  redistribute(Seq(2, 4, 1, 2)) === Seq(3, 1 ,2 ,3)
  redistribute(Seq(3, 1, 2, 3)) === Seq(0, 2, 3, 4)
  redistribute(Seq(0, 2, 3, 4)) === Seq(1, 3, 4, 1)
  redistribute(Seq(1, 3, 4, 1)) === Seq(2, 4, 1, 2)

  private def cycleLength(banks: Seq[Int], visited: Set[Seq[Int]] = Set.empty): (Int, Seq[Int]) =
    if (visited contains banks) (visited.size, banks)
    else {
      val next = redistribute(banks)
      cycleLength(next, visited + banks)
    }

  cycleLength(Seq(0, 2, 7, 0)) === (5, Seq(2, 4, 1, 2))

  override def part1(input: Seq[Int]): Int = cycleLength(input)._1

  private def nextCycleLength(banks: Seq[Int]): Int = {
    val (_, stable) = cycleLength(banks)
    cycleLength(stable)._1
  }

  nextCycleLength(Seq(0, 2, 7, 0)) === 4

  override def part2(input: Seq[Int]): Int = nextCycleLength(input)
}
