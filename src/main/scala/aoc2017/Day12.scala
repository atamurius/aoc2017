package aoc2017

import scala.annotation.tailrec

object Day12 extends Puzzle {

  override type Input = Map[Int, Seq[Int]]

  def parse(line: String): (Int, Seq[Int]) = {
    val Seq(left, right) = line.split(" <-> ").toSeq
    left.toInt -> right.split(", ").map(_.toInt)
  }

  override val input: Map[Int, Seq[Int]] = linesOf("Day12.input").map(parse).toMap

  @tailrec
  def groupOf(nodes: Input, from: Set[Int], visited: Set[Int] = Set.empty): Set[Int] = {
    val next = for {
      start <- from
      node <- nodes(start) if !visited.contains(node)
    } yield node
    if (next.isEmpty) visited
    else groupOf(nodes, next, visited ++ next)
  }

  val testGraph = Map(
    0 -> Seq(2),
    1 -> Seq(1),
    2 -> Seq(0, 3, 4),
    3 -> Seq(2, 4),
    4 -> Seq(2, 3, 6),
    5 -> Seq(6),
    6 -> Seq(4, 5)
  )

  groupOf(testGraph, from = Set(0)) === Set(0, 2, 3, 4, 5, 6)

  override def part1(input: Map[Int, Seq[Int]]): Any = groupOf(input, from = Set(0)).size

  @tailrec
  def groupsOf(nodes: Input, visited: Set[Set[Int]] = Set.empty): Set[Set[Int]] =
    nodes.keys.find { node => ! visited.exists { _ contains node } } match {
      case None => visited
      case Some(node) =>
        val group = groupOf(nodes, from = Set(node))
        groupsOf(nodes, visited + group)
    }

  groupsOf(testGraph) === Set(Set(0, 2, 3, 4, 5, 6), Set(1))

  override def part2(input: Map[Int, Seq[Int]]): Any = groupsOf(input).size
}
