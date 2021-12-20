package aoc2017

import scala.util.{Failure, Success, Try}

object Day25 extends Puzzle {

  case class State(state: Char,
                   position: Int = 0,
                   ones: Set[Int] = Set.empty) {

    def read: Int = if (ones.contains(position)) 1 else 0

    def run(instruction: Instruction): State = State(
      state = instruction.nextState,
      position = position + (if (instruction.moveLeft) -1 else 1),
      ones = if (instruction.writeOne) ones + position else ones - position
    )

    def checksum: Int = ones.size

    override def toString: String = {
      val left = (ones + position).min
      val right = (ones + position).max
      def tape(p: Int) = {
        if (p == position) s"[${if (ones(p)) 1 else 0}]" else s" ${if (ones(p)) 1 else 0} "
      }
      s"$state: $left ... ${(left to right).map(tape).mkString} ... $right"
    }
  }

  case class Instruction(writeOne: Boolean, moveLeft: Boolean, nextState: Char)

  case class Blueprint(start: Char,
                       time: Int,
                       program: Map[(Char, Int), Instruction]) {

    def runOn(state: State): State = state.run(program(state.state, state.read))
  }

  implicit class OptionTry[T](val o: Option[T]) extends AnyVal {
    def orFail(msg: => String): Try[T] = o match {
      case Some(value) => Success(value)
      case None => Failure(new IllegalArgumentException(msg))
    }
  }

  def parse(lines: Iterator[String]): Blueprint = {
    val Begin = """Begin in state ([A-Z]).""".r
    val Time = """Perform a diagnostic checksum after (\d+) step""".r
    val State = """In state ([A-Z]):""".r
    val Value = """If the current value is ([01]):""".r
    val Write = """- Write the value ([01]).""".r
    val Move = """- Move one slot to the (right|left).""".r
    val Next = """- Continue with state ([A-Z]).""".r

    def parseInstruction(value: String, write: String, move: String, cont: String) = for {
      v <- Value.findFirstMatchIn(value) orFail s"Can't match value in '$value'"
      w <- Write.findFirstMatchIn(write) orFail s"Can't match write in '$write'"
      m <- Move.findFirstMatchIn(move) orFail s"Can't match move in '$move'"
      c <- Next.findFirstMatchIn(cont) orFail s"Can't match next in '$cont'"
    } yield v.group(1).toInt -> Instruction(
      writeOne = w.group(1) == "1",
      moveLeft = m.group(1) == "left",
      nextState = c.group(1).head
    )

    val Seq(begin, time, rest @ _*) = lines.toSeq
    log(s"Begin: $begin")
    log(s"Time:  $time")
    Blueprint(
      start = Begin.findFirstMatchIn(begin).getOrElse(sys.error(s"Can't match begin in '$begin'")).group(1).head,
      time = Time.findFirstMatchIn(time).getOrElse(sys.error(s"Can't match time in '$time'")).group(1).toInt,
      program = rest.sliding(10, 10).flatMap {
        case Seq(_, state, value1, w1, m1, c1, value2, w2, m2, c2) =>
          log(s"Parsing `$state`")
          val at = State.findFirstMatchIn(state)
            .getOrElse { sys.error(s"Can't match state in '$state'") }
            .group(1)
            .head
          val (v1, i1) = parseInstruction(value1, w1, m1, c1).get
          val (v2, i2) = parseInstruction(value2, w2, m2, c2).get
          Seq(
            (at, v1) -> i1,
            (at, v2) -> i2
          )
      }.toMap
    )
  }

  def run(blueprint: Blueprint): State = (1 to blueprint.time).foldLeft(State(blueprint.start)) { (state, i) =>
    val next = blueprint.runOn(state)
    log(s"#$i -> $next")
    next
  }

  override type Input = Blueprint

  override val input: Blueprint = parse(linesOf("Day25.input"))

  /*logged*/ {
    run(parse(
      """
        |Begin in state A.
        |Perform a diagnostic checksum after 6 steps.
        |
        |In state A:
        |  If the current value is 0:
        |    - Write the value 1.
        |    - Move one slot to the right.
        |    - Continue with state B.
        |  If the current value is 1:
        |    - Write the value 0.
        |    - Move one slot to the left.
        |    - Continue with state B.
        |
        |In state B:
        |  If the current value is 0:
        |    - Write the value 1.
        |    - Move one slot to the left.
        |    - Continue with state A.
        |  If the current value is 1:
        |    - Write the value 1.
        |    - Move one slot to the right.
        |    - Continue with state A.
      """.stripMargin.trim.linesIterator)).checksum === 3
  }

  override def part1(input: Blueprint): Any = run(input).checksum
}
