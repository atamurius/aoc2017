package aoc2017

object Day24 extends Puzzle {

  case class Component(in: Int, out: Int) {
    def flip = Component(out, in)

    def <~ (bridge: List[Component]): Iterator[List[Component]] = bridge match {

      case Nil if in == 0 => Iterator(List(this))

      case Nil if out == 0 => Iterator(List(flip))

      case Component(_, `in`) :: _ => Iterator(this :: bridge)

      case Component(_, `out`) :: _ => Iterator(flip :: bridge)

      case _ => Iterator.empty
    }

    override def toString = s"$in/$out"

    override def hashCode(): Int = in ^ out

    override def equals(obj: Any): Boolean = obj match {
      case Component(a, b) => (a == in && b == out) || (a == out && b == in)
      case _ => false
    }
  }

  implicit class ComponentBuilder(val in: Int) extends AnyVal {
    def \(out: Int) = Component(in, out)
  }

  def parse(str: String): Component = str.split("/") match { case Array(a, b) => a.toInt \ b.toInt }

  override type Input = Set[Component]

  override val input: Set[Component] = linesOf("Day24.input").map(parse).toSet

  def bridges(cs: Set[Component], bridge: List[Component] = Nil): Seq[List[Component]] =
    if (cs.isEmpty) Nil
    else {
      val growed = for {
        c <- cs.toSeq
        next <- c <~ bridge
        result <- bridges(cs - c, next)
      } yield result
      bridge +: growed
    }

  def strength(b: List[Component]): Int = b.map { case Component(in, out) => in + out }.sum

  def length(b: List[Component]): Int = b.size

  def lengthThenStrength(b: List[Component]): (Int, Int) = length(b) -> strength(b)

  /*logged*/ {
    val components =
      """
        |0/2
        |2/2
        |2/3
        |3/4
        |3/5
        |0/1
        |10/1
        |9/10
      """.stripMargin.trim.linesIterator.map(parse).toSet

    val result = bridges(components)
    result.foreach { b =>
      log(b.reverse.mkString("--"))
    }
    result.toSet === Set(
      Nil,
      0\1 :: Nil,
      0\1 :: 10\1 :: Nil,
      0\1 :: 10\1 :: 9\10 :: Nil,
      0\2 :: Nil,
      0\2 :: 2\3 :: Nil,
      0\2 :: 2\3 :: 3\4 :: Nil,
      0\2 :: 2\3 :: 3\5 :: Nil,
      0\2 :: 2\2 :: Nil,
      0\2 :: 2\2 :: 2\3 :: Nil,
      0\2 :: 2\2 :: 2\3 :: 3\4 :: Nil,
      0\2 :: 2\2 :: 2\3 :: 3\5 :: Nil
    ).map(_.reverse)

    strength(result.max(Ordering by strength)) === 31
    strength(result.max(Ordering by lengthThenStrength)) === 19
  }

  override def part1(input: Set[Component]): Any = strength(bridges(input).max(Ordering by strength))

  override def part2(input: Set[Component]): Any = {
    val result = bridges(input)
    strength(result.max(Ordering by lengthThenStrength))
  }
}
