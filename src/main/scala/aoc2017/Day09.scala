package aoc2017

object Day09 extends Puzzle {

  override type Input = String
  override type Output = Int

  override val input: String = linesOf("Day09.input").next()

  sealed trait Piece
  case class Group(pieces: Piece*) extends Piece
  case class Garbage(content: String) extends Piece

  case class Parser[+T](parse: Parser.Chars => (T, Parser.Chars)) {
    def map[R](f: T => R): Parser[R] = Parser {
      parse(_) match {
        case (t, rest) => (f(t), rest)
      }
    }
    def flatMap[R](f: T => Parser[R]): Parser[R] = Parser {
      parse(_) match {
        case (t, rest) => f(t).parse(rest)
      }
    }
  }

  object Parser {

    type Chars = List[Char]

    def fail(msg: String) = throw new IllegalArgumentException(msg)

    def take(n: Int) = Parser { _.splitAt(n) }
    def just[T](v: T) = Parser { cs => (v, cs) }
    def lookup[T](f: PartialFunction[Chars, Parser[T]]): Parser[T] = Parser { cs => f(cs).parse(cs) }
  }

  object Piece {
    import Parser._

    val GroupStart = '{'
    val GroupEnd = '}'
    val GarbageStart = '<'
    val GarbageEnd = '>'
    val Separator = ','
    val Escape = '!'

    def apply(text: String): Piece = piece.parse(text.toList) match {
      case (res, Nil) => res
      case (_, rest) => fail(s"Unparsed tail: '${rest.mkString}'")
    }

    lazy val piece: Parser[Piece] = lookup {
      case GroupStart :: GroupEnd :: _ => for (_ <- take(2)) yield Group()
      case GroupStart :: _ => for (_ <- take(1); ps <- pieces) yield Group(ps : _*)
      case GarbageStart :: GarbageEnd :: _ => for (_ <- take(2)) yield Garbage("")
      case GarbageStart :: _ => for (_ <- take(1); g <- garbage) yield Garbage(g.mkString)
    }

    lazy val pieces: Parser[List[Piece]] = for {
      head <- piece
      sep <- take(1)
      tail <- sep match {
        case Seq(GroupEnd) => just(Nil)
        case Seq(Separator) => pieces
      }
    } yield head :: tail

    lazy val garbage: Parser[List[Char]] = lookup {
      case GarbageEnd :: _ => for (_ <- take(1)) yield Nil
      case Escape :: _ => for (_ <- take(2); rest <- garbage) yield rest
      case _ => for (cs <- take(1); rest <- garbage) yield cs ++ rest
    }
  }

  Piece("{}") === Group()
  Piece("{{}}") === Group(Group())
  Piece("{{},{},{}}") === Group(Group(),Group(),Group())
  Piece("<>") === Garbage("")
  Piece("{<{},{},{{}}>}") === Group(Garbage("{},{},{{}}"))
  Piece("<!!!>>") === Garbage("")

  private def score(start: Int)(p: Piece): Int = p match {
    case Group(ps @ _*) => (ps map score(start + 1)).sum + start
    case _ => 0
  }

  override def part1(input: String): Int = score(1)(Piece(input))

  private def garbageLen(piece: Piece): Int = piece match {
    case Group(ps @ _*) => (ps map garbageLen).sum
    case Garbage(c) => c.length
  }

  override def part2(input: String): Int = garbageLen(Piece(input))
}
