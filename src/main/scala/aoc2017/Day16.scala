package aoc2017

object Day16 extends Puzzle {

  override type Input = Seq[Move]

  override val input: Seq[Move] = linesOf("Day16.input").next().split(",").map(Move.parse)

  sealed trait Move
  case class Spin(n: Int) extends Move
  case class Exchange(a: Int, b: Int) extends Move
  case class Partner(a: String, b: String) extends Move

  object Move {
    def parse(str: String): Move = str.splitAt(1) match {
      case ("s", n) => Spin(n.toInt)
      case ("x", ab) =>
        val Seq(a, b) = ab.split("/").toSeq
        Exchange(a.toInt, b.toInt)
      case ("p", ab) =>
        val Seq(a, b) = ab.split("/").toSeq
        Partner(a, b)
    }
  }

  def apply(seq: Array[String], move: Move): Array[String] = {
    def reverse(start: Int, end: Int): Unit = {
      var i = start
      var j = end - 1
      while (i < j) {
        swap(i, j)
        i += 1
        j -= 1
      }
    }
    def swap(i: Int, j: Int): Unit = {
      val t = seq(i)
      seq(i) = seq(j)
      seq(j) = t
    }
    move match {
      case Spin(n) => reverse(0, seq.length - n); reverse(seq.length - n, seq.length); reverse(0, seq.length)
      case Exchange(a, b) => swap(a, b)
      case Partner(a, b) => swap(seq indexOf a, seq indexOf b)
    }
    seq
  }

  def applyAll(seq: Array[String], moves: Move*): Array[String] = moves.foldLeft(seq)(apply)

  def letters(last: Char): Array[String] = ('a' to last).map(_.toString).toArray

  applyAll(letters('e'), Spin(3)).mkString === "cdeab"
  applyAll(letters('e'), Spin(1), Exchange(3,4), Partner("e","b")).mkString === "baedc"

  override def part1(input: Seq[Move]): Any = applyAll(letters('p'), input : _*).mkString

  def cycle(arr: Array[String], moves: Move*): (Int, Int) = {
    val visited = collection.mutable.Map.empty[String, Int]
    var i = 0
    while (true) {
      i += 1
      applyAll(arr, moves : _*)
      val curr = arr.mkString
      if (visited contains curr) return visited(curr) -> i
      visited += curr -> i
    }
    (-1, -1)
  }

  def applyTimesLiteraly(n: Int, arr: Array[String], moves: Move*): Array[String] = {
    (1 to n).foreach { _ => applyAll(arr, moves : _*) }
    arr
  }

  def applyTimes(n: Int, arr: Array[String], moves: Move*): Array[String] = {
    val (start, loop) = cycle(arr, moves : _*)
    val rest = (n - start) % (loop - start)
    applyTimesLiteraly(rest, arr, moves : _*)
    arr
  }

//  timed {
//    val (_, end) = cycle(letters('p'), input : _*)
//    applyTimes(end * 2 + 10, letters('p'), input : _*).mkString ===
//      applyTimesLiteraly(end * 2 + 10, letters('p'), input : _*).mkString
//  }

  override def part2(input: Seq[Move]): Any = applyTimes(1000*1000*1000, letters('p'), input : _*).mkString
}
