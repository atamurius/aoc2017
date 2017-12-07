package aoc2017

object Day04 extends Puzzle {

  override type Input = Seq[Seq[String]]
  override type Output = Int

  override val input: Seq[Seq[String]] = linesOf("Day04.input").map { _.split("\\s+").toSeq }.toSeq

  override def part1(input: Seq[Seq[String]]): Int = input count valid

  private def valid(phrase: Seq[String]) =
    phrase.foldLeft(Map.empty[String, Int] withDefaultValue 0) { (map, word) =>
      map updated (word, map(word) + 1)
    }.forall(_._2 == 1)

  valid("aa bb cc dd ee" split " ") === true
  valid("aa bb cc dd ee aa" split " ") === false
  valid("aa bb cc dd ee aaa" split " ") === true

  override def part2(input: Seq[Seq[String]]): Int = input count valid2

  private def anagramize(word: String): String = word.sorted

  private def valid2(phrase: Seq[String]) = valid(phrase map anagramize)

  valid2("abcde fghij" split " ") === true
  valid2("abcde xyz ecdab" split " ") === false
  valid2("a ab abc abd abf abj" split " ") === true
  valid2("iiii oiii ooii oooi oooo" split " ") === true
  valid2("oiii ioii iioi iiio" split " ") === false
}
