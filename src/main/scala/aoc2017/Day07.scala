package aoc2017

object Day07 extends Puzzle {

  override type Input = Seq[Record]

  case class Record(name: String, weight: Int, hold: Seq[String])

  private def parse(lines: Seq[String]) = {
    val RecordPat = """(\w+) \((\d+)\)( -> ([\w, ]+))?""".r
    lines.map {
      case RecordPat(name, weight, _, rest) =>
        Record(name, weight.toInt, Option(rest).map { _.split(", ").toSeq } getOrElse Seq())
    }
  }

  override val input: Seq[Record] = parse(linesOf("Day07.input").toSeq)

  private val testRecords = parse(
    """
      |pbga (66)
      |xhth (57)
      |ebii (61)
      |havc (66)
      |ktlj (57)
      |fwft (72) -> ktlj, cntj, xhth
      |qoyq (66)
      |padx (45) -> pbga, havc, qoyq
      |tknk (41) -> ugml, padx, fwft
      |jptl (61)
      |ugml (68) -> gyxo, ebii, jptl
      |gyxo (61)
      |cntj (57)
    """.trim.stripMargin.lines.toSeq)

  case class Node(name: String,
                  weight: Int,
                  children: Seq[Node] = Seq.empty)

  private def tree(records: Seq[Record]): Node = {
    val rs = records.map { r => r.name -> r }.toMap
    var orphans = rs.keySet
    val nodes = collection.mutable.Map.empty[String, Node]
    def build(name: String): Node = nodes.getOrElseUpdate(name, {
      val Record(_, w, children) = rs(name)
      orphans --= children
      Node(name, w, children map build)
    })
    rs.keys.foreach(build)
    val Seq(top) = orphans.toSeq
    nodes(top)
  }

  tree(testRecords).name === "tknk"

  override def part1(input: Seq[Record]): Any = tree(input).name

  private def unbalanced(tree: Node): Either[Int, (Node, Int)] = { // either wight or unbalanced node
    val start: Either[Map[Node, Int], (Node, Int)] = Left(Map.empty)
    tree.children.foldLeft(start) { (state, child) =>
      for {
        ws <- state.left
        w <- unbalanced(child).left
      } yield ws + (child -> w)
    }.left.flatMap { ws =>
      if (ws.values.forall { _ == ws.values.head })
        Left(ws.values.sum + tree.weight)
      else {
        val sorted = ws.toSeq.sortBy(_._2)
        val size = ws.size
        val common = sorted(size/2)._2
        val (node, w) =
          if (sorted.head._2 != common) sorted.head
          else sorted.last
        Right(node -> Math.abs(node.weight + common - w))
      }
    }
  }

  unbalanced(Node("test", 5)) === Left(5)

  unbalanced(tree(testRecords)).right.map {
    case (node, w) => node.name -> w
  } === Right("ugml" -> 60)

  override def part2(input: Seq[Record]): Any = unbalanced(tree(input)).right.get._2
}