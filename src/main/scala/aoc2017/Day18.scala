package aoc2017

import aoc2017.Day18.Instr.{Rcv, Snd}

import scala.annotation.tailrec

object Day18 extends Puzzle {

  override type Input = Seq[Instr]

  override val input: Seq[Instr] =
    """
      |set i 31
      |set a 1
      |mul p 17
      |jgz p p
      |mul a 2
      |add i -1
      |jgz i -2
      |add a -1
      |set i 127
      |set p 316
      |mul p 8505
      |mod p a
      |mul p 129749
      |add p 12345
      |mod p a
      |set b p
      |mod b 10000
      |snd b
      |add i -1
      |jgz i -9
      |jgz a 3
      |rcv b
      |jgz b -1
      |set f 0
      |set i 126
      |rcv a
      |rcv b
      |set p a
      |mul p -1
      |add p b
      |jgz p 4
      |snd a
      |set a b
      |jgz 1 3
      |snd b
      |set f 1
      |add i -1
      |jgz i -11
      |snd a
      |jgz f -16
      |jgz a -19
      |""".trim.stripMargin.linesIterator.map(Instr.parse).toSeq

  type Reg = String
  type Val = Either[Long, Reg]

  sealed trait Instr extends Product {
    override def toString: String = {
      val name = getClass.toString
      val parts = name.substring(name.lastIndexOf('$') + 1).toLowerCase +: productIterator.map {
        case v: Either[_, _] => v.fold(identity, identity)
        case x => x
      }.toSeq
      parts mkString " "
    }
  }

  object Instr {

    case class Snd(value: Val) extends Instr

    case class Set(dest: Reg, value: Val) extends Instr

    case class Add(dest: Reg, value: Val) extends Instr

    case class Mul(dest: Reg, value: Val) extends Instr

    case class Mod(dest: Reg, value: Val) extends Instr

    case class Rcv(value: Val) extends Instr

    case class Jgz(test: Val, offset: Val) extends Instr

    def parse(str: String): Instr = str.split(" ").toSeq match {
      case Seq("snd", v)    => Snd(parseVal(v))
      case Seq("set", r, v) => Set(r, parseVal(v))
      case Seq("add", r, v) => Add(r, parseVal(v))
      case Seq("mul", r, v) => Mul(r, parseVal(v))
      case Seq("mod", r, v) => Mod(r, parseVal(v))
      case Seq("rcv", v)    => Rcv(parseVal(v))
      case Seq("jgz", t, o) => Jgz(parseVal(t), parseVal(o))
      case _ => sys.error(s"Failed parsing instruction $str")
    }

    private def parseVal(str: String): Val =
      Either.cond(str.head.isLetter, str, str.toLong)
  }

  private val testSeq =
    """
      |set a 1
      |add a 2
      |mul a a
      |mod a 5
      |snd a
      |set a 0
      |rcv a
      |jgz a -1
      |set a 1
      |jgz a -2
    """.trim.stripMargin.linesIterator.map(Instr.parse).toSeq

  case class Program(code: Seq[Instr]) {
    private var pos = 0
    private var regs = Map.empty[String, Long] withDefaultValue 0L

    def logState(): Unit = {
      regs.foreach { case (k, v) =>
          log(s"$k: $v")
      }
      log("---------------")
      code.zipWithIndex.foreach { case (instr, i) =>
        log(s"${if (i == pos) ">" else " "} $instr")
      }
    }

    import Instr._

    def valueOf(v: Val): Long = v.fold(identity, regs)
    def update(reg: String, value: Long): Unit = regs += reg -> value

    private def regUpdate(r: String)(f: Long => Long): Unit = regs += r -> f(regs(r))

    private val seqCommands: PartialFunction[Instr, Unit] = {
      case Set(d, v) => regUpdate(d) { _ => valueOf(v) }
      case Add(d, v) => regUpdate(d) { _ + valueOf(v) }
      case Mul(d, v) => regUpdate(d) { _ * valueOf(v) }
      case Mod(d, v) => regUpdate(d) { _ % valueOf(v) }
    }

    private val jumpCommands: PartialFunction[Instr, Int] = {
      case Jgz(test, offset) => if (valueOf(test) > 0) valueOf(offset).toInt else 1
    }

    private val execute = seqCommands.andThen(_ => 1) orElse jumpCommands

    @tailrec
    final def run(): Option[Instr] =
      if (! code.indices.contains(pos)) None
      else code(pos) match {
        case i @ (Snd(_) | Rcv(_)) =>
          pos += 1
          Some(i) // interrupt
        case i =>
          pos += execute(i)
          run()
      }
  }

  @tailrec
  private def runSelfReceive(p: Program, sound: Long = 0): Option[Long] =
    p.run() match {
      case None => None
      case Some(Snd(v)) => runSelfReceive(p, p.valueOf(v))
      case Some(Rcv(v)) if p.valueOf(v) != 0 => Some(sound)
      case _ => runSelfReceive(p, sound)
    }

  runSelfReceive(Program(testSeq)) === Some(4)

  override def part1(input: Seq[Instr]): Any = runSelfReceive(Program(input)).get

  private val secondTestSeq =
    """
      |snd 1
      |snd 2
      |snd p
      |rcv a
      |rcv b
      |rcv c
      |rcv d
    """.trim.stripMargin.linesIterator.map(Instr.parse).toSeq

  case class Thread(code: Seq[Instr], pid: Int) {
    val program = Program(code)
    program("p") = pid

    var queue: Seq[Long] = Seq.empty

    var isStopped = false
    var isWaiting = Option.empty[String]

    var sendCount = 0

    def isActive = !isStopped && isWaiting.isEmpty

    def run(): Option[Long] = program.run() match {
      case None => isStopped = true; None
      case Some(Snd(v)) =>
        sendCount += 1
        Some(program.valueOf(v))
      case Some(Rcv(v)) if queue.nonEmpty =>
        program(v.right.get) = queue.last
        queue = queue.dropRight(1)
        None
      case Some(Rcv(v)) =>
        isWaiting = Some(v.right.get)
        None
    }

    def send(v: Long): Unit = isWaiting match {
      case None => queue = v +: queue
      case Some(r) =>
        program(r) = v
        isWaiting = None
    }

    def logState(): Unit = {
      log(s"=== #$pid ${if (isStopped) "STOPPED" else s"waiting: $isWaiting"}, queue: $queue")
      program.logState()
    }
  }

  private def runParallel(code: Seq[Instr]): Int = {
    val first = Thread(code, 0)
    val second = Thread(code, 1)
    def run(active: Thread, pending: Thread): Unit = active.run() match {
      case None => ()
      case Some(v) => pending.send(v)
    }
    while (! second.isStopped) {
//      first.logState()
//      second.logState()
      if (! first.isActive && ! second.isActive) {
        log("Dead lock")
        first.isStopped = true
        second.isStopped = true
      }
      else {
        if (first.isActive) run(first, second)
        if (second.isActive) run(second, first)
      }
    }
    second.sendCount
  }

  runParallel(secondTestSeq) === 3

  override def part2(input: Seq[Instr]): Any = runParallel(input)
}
