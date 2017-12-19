package aoc2017

object Day15 extends Puzzle {

  override type Input = (Long, Long)

  override val input: (Long, Long) = (634, 301)

  private val factors = (16807, 48271) // 7^5, 48271

  private val modulo = 2147483647 // 2^31 - 1

  private def generator(start: Long, factor: Int, modulo: Int = modulo) =
    Iterator.iterate(start) { x => (x * factor) % modulo }.drop(1)

  generator(65L, factors._1).take(5).toSeq ===
    Seq(1092455, 1181022009, 245556042, 1744312007, 1352636452)

  private val l40m = 40 * 1000 * 1000

  private def same(x: Long, y: Long) = (x & 0xffff) == (y & 0xffff)

  private def generators(s: (Long, Long)) =
    generator(s._1, factors._1) zip generator(s._2, factors._2)

//  count((65L, 8921L), factors, l40m)(same) === 588

  override def part1(input: (Long, Long)): Any =
    generators(input)
    .take(l40m)
    .count(same _ tupled)

//  logged { count2((65L, 8921L), factors, 5) }

  private def generator2(start: Long, factor: Int, k: Int, modulo: Int = modulo) =
    generator(start, factor, modulo).filter { x => (x % k) == 0 }

  private def generators2(s: (Long, Long)) =
    generator2(s._1, factors._1, 4) zip generator2(s._2, factors._2, 8)

  generator2(65, factors._1, 4).take(5).toList ===
    Seq[Long](1352636452, 1992081072, 530830436, 1980017072, 740335192)

  generators2((65L, 8921L)).take(1056).count(same _ tupled) === 1

  override def part2(input: (Long, Long)): Any =
    generators2(input)
      .take(5 * 1000 * 1000)
      .count(same _ tupled)
}
