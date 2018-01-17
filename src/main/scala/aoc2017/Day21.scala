package aoc2017

import scala.annotation.tailrec

object Day21 extends Puzzle {

  override type Input = Seq[(Block, Block)]

  override val input: Seq[(Block, Block)] = linesOf("Day21.input").map(rule).toSeq

  case class Block(self: List[List[Char]]) extends AnyVal {
    def size: Int = self.size

    def count(c: Char): Int = self.iterator.flatMap(_.iterator).count(_ == c)

    def rotate: Block = self match {
      // a b => c a
      // c d    d b
      case List(a, b) :: List(c, d) :: Nil => Block(List(c, a) :: List(d, b) :: Nil)
      // a b c => g d a
      // d e f    h e b
      // g h i    i f c
      case List(a, b, c) :: List(d, e, f) :: List(g, h, i) :: Nil => Block(
        List(g, d, a) :: List(h, e, b) :: List(i, f, c) :: Nil
      )
      case _ => sys.error(s"Unsupported block of size $size")
    }
    def flipH: Block = Block(self.map(_.reverse))

    def flipV: Block = Block(self.reverse)

    def permutations: Set[Block] = {
      val rotated = Iterator.iterate(this)(_.rotate).take(4).toSet
      rotated ++ rotated.map(_.flipV) ++ rotated.map(_.flipH)
    }

    def mkString(sep: String = "/"): String = self.map(_.mkString).mkString(sep)

    override def toString: String = mkString()

    def divideBy(sz: Int): Seq[Block] = {
      val n = size / sz
      for {
        y <- 0 until n
        x <- 0 until n
      } yield Block(
        self.slice(y*sz, y*sz + sz).map(_.slice(x*sz, x*sz + sz))
      )
    }
  }

  private def block(str: String) = Block(str.split("/").toList.map(_.toList))

  private def blocks(strs: String*) = strs.map(block)

  private def rule(str: String): (Block, Block) = {
    val Array(a, b) = str.split(" => ")
    block(a) -> block(b)
  }

  private def merge(bs: Seq[Block]): Block = {
    val n = math.sqrt(bs.size).toInt
    // xx xx
    // xx xx
    Block(
      bs.grouped(n).flatMap { rowBlocks =>
        rowBlocks.map(_.self).reduceLeft { (l, r) =>
          (l zip r).map { case (a, b) => a ++ b }
        }
      }.toList
    )
  }

  // AB BA CD CA DC BD
  // CD DC AB DB BA AC
//  block("AB/CD").permutations === blocks("AB/CD", "BA/DC", "CD/AB", "CA/DB", "DC/BA", "BD/AC").toSet

  block("ABCD/1234/EFGH/5678").divideBy(2) === blocks("AB/12", "CD/34", "EF/56", "GH/78")

  merge(blocks("AB/12", "CD/34", "EF/56", "GH/78")) === block("ABCD/1234/EFGH/5678")

  private def transform(b: Block, rules: Seq[(Block, Block)]): Block = {
    val bs = if (b.size % 2 == 0) b.divideBy(2) else b.divideBy(3)
    log("---------------------")
    log(s"Divided to $bs")
    merge(bs.map { block =>
      val ps = block.permutations
      rules.find { case (p, _) => p.size == block.size && ps.contains(p) } match {
        case None => sys.error(s"No matched rule for $block with permutations $ps")
        case Some((_, result)) => result
      }
    })
  }

  @tailrec
  private def transformN(n: Int)(b: Block, rules: Seq[(Block, Block)]): Block =
    if (n == 0) b
    else transformN(n - 1)(transform(b, rules), rules)

  private val initial = block(".#./..#/###")

  //  logged {
    transformN(2)(initial, Seq(
      rule("../.# => ##./#../..."),
      rule(".#./..#/### => #..#/..../..../#..#")
    )) === block("##.##./#..#../....../##.##./#..#../......")
//  }

  override def part1(input: Seq[(Block, Block)]): Any = transformN(5)(initial, input).count('#')

  override def part2(input: Seq[(Block, Block)]): Any = transformN(18)(initial, input).count('#')
}
