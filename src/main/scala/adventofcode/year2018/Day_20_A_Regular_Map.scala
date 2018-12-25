package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec

case object Day_20_A_Regular_Map extends Puzzle {

  sealed trait Regex
  case object Empty extends Regex
  case class Literal(value: Seq[Char]) extends Regex
  case class Sequence(seq: Regex*) extends Regex
  case class Choice(options: Regex*) extends Regex

  def toRegex(str: String): Regex = {
    require(str.startsWith("^") && str.endsWith("$"))

    def literal(i: Int): (Literal, Int) = str.indexWhere("()|$".toSet, i) match {
      case j => (Literal(str.substring(i, j)), j)
    }

    @tailrec
    def choice(i: Int, options: Vector[Regex] = Vector.empty): (Choice, Int) = str(i) match {
      case ')' => (Choice(options: _*), i + 1)
      case '('|'|' => sequence(i + 1) match {
        case (option, j) => choice(j, options :+ option)
      }
    }

    @tailrec
    def sequence(i: Int, vs: Vector[Regex] = Vector.empty): (Regex, Int) = str(i) match {
      case '$'|')'|'|' =>
        if (vs.isEmpty) (Empty, i)
        else if (vs.size == 1) (vs.head, i)
        else (Sequence(vs: _*), i)
      case _ => regex(i) match {
        case (v, j) => sequence(j, vs :+ v)
      }
    }

    def regex(i: Int): (Regex, Int) = str(i) match {
      case '(' => choice(i)
      case  _  => literal(i)
    }

    sequence(1) match {
      case (res, j) if str(j) == '$' => res
      case other => sys.error(s"Non-full parse: $other")
    }
  }

  toRegex("^WNE$") shouldBe Literal("WNE")
  toRegex("^(WN|E)$") shouldBe Choice(Literal("WN"), Literal("E"))
  toRegex("^ENWWW(NEEE|SSE(EE|N))$") shouldBe Sequence(
    Literal("ENWWW"),
    Choice(
      Literal("NEEE"),
      Sequence(
        Literal("SSE"),
        Choice(Literal("EE"), Literal("N"))
      )
    )
  )
  toRegex("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$") shouldBe Sequence(
    Literal("ENNWSWW"),
    Choice(Literal("NEWS"), Empty),
    Literal("SSSEEN"),
    Choice(Literal("WNSE"), Empty),
    Literal("EE"),
    Choice(Literal("SWEN"), Empty),
    Literal("NNN")
  )

  def generate[T](r: Regex, init: T, middle: Boolean = false)(f: (T, Char) => T): Iterator[T] = r match {
    case Empty => Iterator(init)
    case Literal(cs) => Iterator(cs.foldLeft(init)(f))
    case Sequence(vs @ _*) => vs.zipWithIndex.foldLeft(Iterator(init)) {
      case (acc, (v, i)) => for {
        pref <- acc
        next <- generate(v, pref, middle = i != vs.size - 1)(f)
      } yield next
    }
    case Choice(opts @ _*) =>
      if (middle) {
        opts.foreach(generate(_, init)(f))
        generate(opts.head, init)(f)
      }
      else opts.iterator.flatMap(generate(_, init)(f))
  }

  def generateStrings(r: Regex): Seq[String] = generate(r, "")(_ + _).toList

  generateStrings(toRegex("^WNE$")) shouldBe Seq("WNE")
  generateStrings(toRegex("^(WN|E)$")) shouldBe Seq("WN","E")
  generateStrings(toRegex("^ENWWW(NEEE|SSE(EE|N))$")) shouldBe Seq("ENWWWNEEE","ENWWWSSEEE","ENWWWSSEN")

  class Rooms(regex: Regex) {
    val dirs = Map(
      'W' -> (-1,0),
      'N' -> (0,-1),
      'S' -> (0,+1),
      'E' -> (+1,0)
    )
    var info = Map((0,0) -> 'X')
    var distance = Map((0,0) -> 0)

    generate(regex, (0,0)) { (source, dir) =>
      val (x, y) = source
      val (dx, dy) = dirs(dir)
      val dest = (x + dx*2, y + dy*2)
      val door = (x+dx, y+dy)
      if (! info.contains(door)) {
        info += door -> '+'
        info += dest -> '@'
        val dist = distance(source) + 1
        if (distance.get(dest).forall(_ > dist))
          distance += dest -> dist
//        Debug(distance.size)
      }
      dest
    }.size

    def debug(): Unit = {
      val row = info.keys.map(_._1).min to info.keys.map(_._1).max
      for (y <- info.keys.map(_._2).min to info.keys.map(_._2).max) {
        Debug(row.map(x =>
          distance.get((x,y)).orElse(info.get((x,y))).fold("  ")(v => f"$v%-2s")
        ).mkString(" "))
      }
    }

    def furthest: Int = distance.values.max

    def countAtLeast(dist: Int): Int = distance.values.count(_ >= dist)
  }

  new Rooms(toRegex("^WNE$")).furthest shouldBe 3
  new Rooms(toRegex("^ENWWW(NEEE|SSE(EE|N))$")).furthest shouldBe 10
  new Rooms(toRegex("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")).furthest shouldBe 18
  new Rooms(toRegex("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")).furthest shouldBe 23
  new Rooms(toRegex("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")).furthest shouldBe 31

  override type Input = Regex

  override def input: Input = toRegex(inputLines.mkString)

  override def answer1: Answer = new Rooms(_).furthest

  override def answer2: Answer = new Rooms(_).countAtLeast(1000)
}
