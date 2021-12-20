package aoc2017

object Day17 extends Puzzle {

  override type Input = Int

  override val input = 363

  trait BufferLike {
    def size: Int

    def at(i: Int): Int = (i + size) % size

    def next(v: Int): this.type

    def apply(ns: Iterable[Int]): this.type = ns.foldLeft[this.type](this) { _ next _ }
  }

  case class Buffer(steps: Int) extends BufferLike {
    private val xs = new java.util.LinkedList[Int]
    xs.add(0)

    def size: Int = xs.size

    def apply(i: Int): Int = xs.get(at(i))

    def insertAfter(i: Int, v: Int): Int = {
      val j = at(i + 1)
      xs.add(j, v)
      j
    }

    private var pos = 0

    def next: Int = apply(pos + 1)

    def next(v: Int): this.type = {
      pos = insertAfter(pos + steps, v)
      this
    }

    def toSeq: Seq[Int] = (pos until pos + xs.size).map(apply)
  }

  Buffer(3)(1 to 3).toSeq === Seq(3, 1, 0, 2)
  Buffer(3)(1 to 9).toSeq === Seq(9, 5, 7, 2, 4, 3, 8, 6, 1, 0)
  Buffer(3)(1 to 2017).toSeq.take(4) === Seq(2017, 638, 1513, 851)

  override def part1(input: Int): Any = Buffer(input)(1 to 2017).next

  case class SparseBuffer(steps: Int) extends BufferLike {
    private var pos = 0
    private var count = 1
    private var zeroPos = 0
    private var next = 0

    def size: Int = count

    def afterZero: Int = next

    def next(v: Int): this.type = {
      val insertPos = at(pos + steps)
      count += 1
      if (insertPos == zeroPos) next = v
      if (insertPos < zeroPos) zeroPos += 1
      pos = insertPos + 1
      this
    }
  }

  SparseBuffer(3)(1 to 3).afterZero === 2
  SparseBuffer(3)(1 to 9).afterZero === 9

  override def part2(input: Int): Any = SparseBuffer(input)(1 to 50000000).afterZero
}
