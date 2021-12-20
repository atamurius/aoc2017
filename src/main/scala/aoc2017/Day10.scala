package aoc2017

import scala.annotation.tailrec

object Day10 extends Puzzle {

  override type Input = Seq[Int]

  override val input: Seq[Int] = Seq(189,1,111,246,254,2,0,120,215,93,255,50,84,15,94,62)

  class CircularArray[T: Manifest](val size: Int) {
    private val elements = new Array[T](size)

    @tailrec
    private def mod(i: Int, m: Int): Int =
      if (i < 0) mod(i + m, m)
      else i % m

    def apply(i: Int) = elements(mod(i, size))
    def update(i: Int, value: T): Unit = elements(mod(i, size)) = value

    override def toString: String = elements.mkString("[",", ", "]")

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: CircularArray[_] => that.elements sameElements this.elements
      case _ => false
    }

    def reverse(start: Int, length: Int): this.type = {
      log(s"reverse($start, $length)")
      if (length > 1) {
        var left = mod(start, size)
        var right = mod(start + length - 1, size)
        var middle = false
        while (! middle) {
          log(s"$left -> <- $right")
          val t = this(left)
          this(left) = this(right)
          this(right) = t
          left = mod(left + 1, size)
          middle ||= left == right
          right = mod(right - 1, size)
          middle ||= left == right
        }
      }
      this
    }

    def toSeq: Seq[T] = elements
  }

  object CircularArray {
    def apply[T: Manifest](ts: T*): CircularArray[T] = {
      val arr = new CircularArray[T](ts.size)
      ts.zipWithIndex.foreach {
        case (x, i) => arr(i) = x
      }
      arr
    }
  }

  private def nums(sz: Int) = CircularArray(0 until sz : _*)

  nums(4) === CircularArray(0, 1, 2, 3)
  nums(4).reverse(0, 4) === CircularArray(3, 2, 1, 0)
  nums(4).reverse(0, 3) === CircularArray(2, 1, 0, 3)
  nums(4).reverse(1, 3) === CircularArray(0, 3, 2, 1)
  nums(4).reverse(2, 3) === CircularArray(2, 1, 0, 3)
  // 0:1:2):(3 -> 3:0:1:2 -> 2:1:0:3 -> 1:0:3:2
  nums(4).reverse(3, 4) === CircularArray(1, 0, 3, 2)

  @tailrec
  private def hash[T: Manifest](
      arr: CircularArray[T],
      xs: Seq[Int],
      start: Int = 0,
      skip: Int = 0
  ): arr.type = xs match {
    case Seq() => arr
    case Seq(x, xs @ _*) =>
      arr.reverse(start, x)
      log(s"start: $start, skip: $skip, length: $x -> $arr")
      hash(arr, xs, start + x + skip, skip + 1)
  }

  hash(nums(5), Seq(3, 4, 1, 5)) === CircularArray(3, 4, 2, 1, 0)

  override def part1(input: Seq[Int]): Any = {
    val h = hash(nums(256), input)
    h(0) * h(1)
  }

  private def codes(str: String): Seq[Int] = str.toCharArray.map(_.toInt)

  codes("1,2,3") === Seq(49, 44, 50, 44, 51)

  private val postfix = Seq(17, 31, 73, 47, 23)

  private def hexHash(seq: Seq[Int], window: Int = 16): Seq[Int] =
    seq.foldLeft((List(0), 0)) { (state, x) => state match {
      case (list, `window`) => (x :: list, 1)
      case (curr :: rest, count) => ((curr ^ x) :: rest, count + 1)
    }}._1.reverse

  hexHash(Seq(1,2,3,4,5,6,7,8,9), 3) === Seq(1^2^3, 4^5^6, 7^8^9)

  private def strongHash(str: String) =
    knotHash(str).map { x => "%02x".format(x) }.mkString

  def knotHash(str: String): Seq[Int] = {
    val lengths = codes(str) ++ postfix
    log(s"strongHash($lengths)")
    val l64 = (2 to 64).foldLeft(lengths) { (l, _) => l ++ lengths }
    log(s"initial len = ${lengths.size}, x64 length = ${l64.size}")
    val hashed = hash(nums(256), l64)
    log(s"hashed = $hashed")
    val hex = hexHash(hashed.toSeq)
    log(s"hex = $hex")
    hex
  }

  strongHash("")          === "a2582a3a0e66e6e86e3812dcb672a272"
  strongHash("AoC 2017")  === "33efeb34ea91902bb2f59c9920caa6cd"
  strongHash("1,2,3")     === "3efbe78a8d82f29979031a4aa0b16a9d"
  strongHash("1,2,4")     === "63960835bcdc130f0b66d7ff4f6a5a8e"

  override def part2(input: Seq[Int]): Any = strongHash(input.mkString(","))
}
