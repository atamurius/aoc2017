package aoc2017

object Day08 extends Puzzle {

  override type Input = Seq[Instr]

  case class Instr(target: String, delta: Int, source: String, cond: Int => Boolean)

  override val input: Seq[Instr] = parse(linesOf("Day08.input").toSeq)

  private def parse(lines: Seq[String]) = {
    val operations = Map[String, (Int, Int) => Boolean](
      ">"  -> (_ >  _),
      "<"  -> (_ <  _),
      ">=" -> (_ >= _),
      "<=" -> (_ <= _),
      "==" -> (_ == _),
      "!=" -> (_ != _)
    )
    val Pat = """(\w+) (inc|dec) ([+-]?\d+) if (\w+) (\S+) ([+-]?\d+)""".r

    lines.map {
      case Pat(name, op, v, s, cond, arg) => Instr(
        target = name,
        delta = if (op == "inc") v.toInt else -v.toInt,
        source = s,
        cond = operations.get(cond) map { op => op(_: Int, arg.toInt) } getOrElse {
          throw new IllegalArgumentException(s"Unknown condition `$cond`")
        }
      )
    }
  }

  private def executeOne(state: Map[String, Int], instr: Instr): Map[String, Int] = {
    import instr._
    if (! cond(state(source))) state
    else state updated (target, state(target) + delta)
  }

  private def execute(instr: Seq[Instr]) = {
    val regs = Map.empty[String, Int] withDefaultValue 0
    instr.foldLeft(regs)(executeOne)
  }

  private val testData = parse(
    """
      |b inc 5 if a > 1
      |a inc 1 if b < 5
      |c dec -10 if a >= 1
      |c inc -20 if c == 10
    """.trim.stripMargin.linesIterator.toSeq)

  execute(testData) === Map("c" -> -10, "a" -> 1)

  override def part1(input: Seq[Instr]): Any = execute(input).values.max

  private def maxDuringExecution(instr: Seq[Instr]) = {
    val regs = Map.empty[String, Int] withDefaultValue 0
    instr.foldLeft((regs, 0)) { (prev, i) =>
      val (state, mx) = prev
      val next = executeOne(state, i)
      val nextMx = if (next.isEmpty) mx else next.values.max max mx
      (next, nextMx)
    }._2
  }

  maxDuringExecution(testData) === 10

  override def part2(input: Seq[Instr]): Any = maxDuringExecution(input)
}
