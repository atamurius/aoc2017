package aoc2017

object Day23 extends Puzzle {

  sealed trait Arg

  case class Val(value: Int) extends Arg

  case class Reg(name: Char) extends Arg

  def isReg(str: String): Boolean = str.length == 1 && Character.isAlphabetic(str.head)

  def parseArg(str: String): Arg =
    if (isReg(str)) Reg(str.head)
    else Val(str.toInt)

  parseArg("-12") === Val(-12)
  parseArg("a") === Reg('a')


  sealed trait Cmd extends Product

  abstract class UpdCmd(val op: (Int, Int) => Int) extends Cmd {
    val dest: Reg
    val value: Arg
  }

  case class Set(dest: Reg, value: Arg) extends UpdCmd((_, x) => x)

  case class Sub(dest: Reg, value: Arg) extends UpdCmd(_ - _)

  case class Mul(dest: Reg, value: Arg) extends UpdCmd(_ * _)

  case class Jnz(value: Arg, offset: Arg) extends Cmd

  def parseCmd(str: String): Cmd = str.split(" ").toSeq match {
    case Seq("set", a, b) if isReg(a) => Set(Reg(a.head), parseArg(b))
    case Seq("sub", a, b) if isReg(a) => Sub(Reg(a.head), parseArg(b))
    case Seq("mul", a, b) if isReg(a) => Mul(Reg(a.head), parseArg(b))
    case Seq("jnz", a, b) => Jnz(parseArg(a), parseArg(b))
    case _ => sys.error(s"Invalid command `$str`")
  }

  parseCmd("set a 12") === Set(Reg('a'), Val(12))


  override type Input = Seq[Cmd]

  override val input: Seq[Cmd] = linesOf("Day23.input").map(parseCmd).toSeq

  type Env = Map[String, Int]

  val P = Reg('#')

  implicit def eval(arg: Arg)(implicit env: Env): Int = arg match {
    case Val(v) => v
    case Reg(r) => env(r.toString)
  }

  implicit class LValReg(val reg: Reg) extends AnyVal {
    def *=(f: Int => Int)(implicit env: Env): (String, Int) = reg.name.toString -> f(env(reg.name.toString))
  }

  implicit def int2val(v: Int): Val = Val(v)

  implicit def char2reg(c: Char): Reg = Reg(c)

  def update(reg: String)(f: Int => Int)(implicit env: Env): (String, Int) = reg -> f(env(reg))

  implicit def regName(reg: Reg): String = reg.name.toString

  def eval(cmd: Cmd)(implicit env: Env): Env = cmd match {

    case upd: UpdCmd => env +
      update(upd.dest) { upd.op(_, upd.value) } +
      update(upd.productPrefix) { _ + 1 } +
      update(P) { _ + 1 }

    case Jnz(value, offset) if (value: Int) != 0 => env +
      update(cmd.productPrefix) { _ + 1 } +
      update(P) { _ + offset }

    case _ => env +
      update(P) { _ + 1 }
  }

  def empty: Env = Map.empty withDefaultValue 0

  def eval(code: Seq[Cmd], start: Env = empty): Env = {
    implicit var env: Env = start
    while (code.indices.contains(P: Int)) {
      val cmd = code(P)
      log(s"@ $cmd ~ ${env.filterKeys(_.length == 1)}")
      env = eval(cmd)
    }
    log(s"-> $env")
    env
  }

  eval(Set(Reg('a'), Val(12)))(empty) === Map("a" -> 12, "#" -> 1, "Set" -> 1)

  eval(
    """
      |set a 5
      |set b 1
      |sub a b
      |jnz a -1
    """.stripMargin.trim.lines.map(parseCmd).toSeq
  )("Sub") === 5

  override def part1(input: Seq[Cmd]): Any = eval(input)("Mul")

  /*logged*/ {
    val code = input.zipWithIndex

    val gotos = code.collect {
      case (Jnz(Val(1), Val(off)), line) => line + off
    }
    val loops = code.collect {
      case (Jnz(Reg(_), Val(off)), line) if off < 0 => (line + off, line - 1)
    }.sortBy(_._1)

    def showArg(arg: Arg) = arg match {
      case Reg(name) => name.toString
      case Val(v) => v.toString
    }

    def labels(line: Int, indent: String): Unit = {
      val l = gotos.indexOf(line)
      if (l >= 0) log(s"${indent}label$l:")
    }

    val IND = "   "

    show(code.toList)

    def show(cs: List[(Cmd, Int)], indent: String = ""): Unit = if (cs.nonEmpty) {
      val (_, line) = cs.head
      val addIndent = loops.foldLeft(indent) {
        case (ind, (a, b)) if a <= line && line <= b =>
          if (a == line) {
            log("")
            log(s"${ind}do:")
          }
          s"$ind$IND"
        case (ind, _) => ind
      }
      labels(cs.head._2, indent)
      cs match {
        // Regs update
        case (c: UpdCmd, _) :: _ =>
          val (mods, rest) = cs.span {
            case (cmd: UpdCmd, _) if cmd.dest == c.dest => true
            case _ => false
          }
          val setIndex = mods.lastIndexWhere { case (x, _) => x.isInstanceOf[Set] }
          val modStrs = mods.drop(setIndex max 0).flatMap {
            case (Set(_, value), _) => Seq(showArg(value))
            case (Mul(_, value), _) => Seq("*", showArg(value))
            case (Sub(_, Val(x)), _) if x < 0 => Seq("+", (-x).toString)
            case (Sub(_, value), _) => Seq("-", showArg(value))
          }
          log(s"$addIndent${c.dest.name} :=${if (setIndex >= 0) "" else s" ${c.dest.name}"} ${modStrs.mkString(" ")}")
          show(rest, indent)

        // if's
        case (Jnz(Reg(cond), Val(2)), _) :: (Jnz(Val(1), Val(n)), l2) :: rest =>
          labels(l2, addIndent)
          log(s"${addIndent}if $cond:")
          val (block, other) = rest.splitAt(n - 1)
          show(block, s"$indent$IND")
          log("")
          show(other, indent)

        // if not's
        case (Jnz(Reg(cond), Val(2)), _) :: op :: rest =>
          log(s"${addIndent}if not $cond:")
          show(List(op), s"$indent$IND")
          log("")
          show(rest, indent)

        // goto's
        case (Jnz(Val(1), Val(off)), _) :: rest =>
          log(s"${addIndent}goto label${gotos.indexOf(line + off)}")
          show(rest, indent)

        // while's
        case (Jnz(Reg(cond), Val(off)), _) :: rest if off < 0 =>
          log(s"${addIndent}while $cond")
          log("")
          show(rest, indent)

        case (cmd, _) :: rest =>
          log(s"$addIndent$cmd")
          show(rest, indent)
      }
    }
  }

  /*
[LOG] b := 99
[LOG] c := b
[LOG] if a:
[LOG]    b := b * 100 + 100000 // 109900
[LOG]    c := b + 17000 // 126900
[LOG]
[LOG] label0: // 109900 to 126900 by 17 = 1000 times
[LOG]
[LOG] f := true
[LOG] d := 2
[LOG]
[LOG] do: // 2 to 109900 = 109898 times
[LOG]    e := 2
[LOG]
[LOG]    do: // 2 to 109900 = 109898 times
[LOG]       if d * e == b:
[LOG]          f := false
[LOG]
[LOG]       e := e + 1
[LOG]    while e != b
[LOG]
[LOG]    d := d + 1
[LOG] while d != b
[LOG]
[LOG] if not f:
[LOG]    h := h + 1 // count of non-primes between 109900 and 126900 by 17
[LOG]
[LOG] if b != c:
[LOG]    b := b + 17
[LOG]    goto label0
[LOG]
   */

  val primes: Stream[Int] = 2 #:: Stream.from(3, 2).filter(isPrime)

  def isPrime(n: Int): Boolean = primes.takeWhile { p => p*p <= n }.forall { n % _ != 0 }

  /*logged*/ {
    log(s"Primes: ${primes take 20 mkString ", "}")
  }

  {/*
    val code = input updated
      (6, Set('b', 99)) updated
      (7, Set('c', 99 + 170))
    val res = eval(code, empty + ("a" -> 1))
    logged {
      log(s"h = ${res("h")}, primes = ${primes
        .dropWhile(_ < 99)
        .takeWhile(_ < 270)
        .filter(p => (p - 99) % 17 == 0)
        .mkString(", ")}")
    }
  */}

  override def part2(input: Seq[Cmd]): Any = {
    val b = 99 * 100 + 100000
    val c = b + 17000
    val primesInRange = primes
      .dropWhile { _ < b }
      .takeWhile { _ <= c }
      .filter { p => (p - b) % 17 == 0 }
    (c - b) / 17 + 1 - primesInRange.size
  }
}
