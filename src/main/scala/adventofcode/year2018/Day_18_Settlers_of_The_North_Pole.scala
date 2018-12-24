package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec

case object Day_18_Settlers_of_The_North_Pole extends Puzzle {

  sealed abstract class State(val sign: Char)
  case object OpenGround extends State('.')
  case object Trees extends State('|')
  case object Lumberyard extends State('#')

  type Transformer = PartialFunction[(State, Map[State, Int]), State]

  class Terrain(initial: Seq[Seq[State]]) {
    private var state = initial.toVector.map(_.toVector)

    def neiboursOf(row: Int, col: Int): Map[State, Int] = {
      val ns = for {
        r <- (row - 1 to row + 1).iterator if r >= 0 && r < state.size
        c <-  col - 1 to col + 1 if (r != row || c != col) && c >= 0 && c < state.head.size
      } yield state(r)(c)
      ns.foldLeft(Map.empty[State, Int] withDefaultValue 0) { (m, s) =>
        m + (s -> (m(s) + 1))
      }
    }

    def transform(f: Transformer): this.type = {
      state = state.iterator.zipWithIndex.map { case (row, r) =>
        row.iterator.zipWithIndex.map {
          case (current, c) => f.lift((current, neiboursOf(r, c))).getOrElse(current)
        }.toVector
      }.toVector
      this
    }

    def debug(): Unit = {
      state.foreach { row =>
        Debug(row.iterator.map(_.sign).mkString)
      }
      Debug("")
    }

    def count(s: State): Int = state.foldLeft(0)(_ + _.count(_ == s))

    @tailrec
    final def detectLoop(t: Transformer, now: Int = 0, history: Map[String, Int] = Map.empty): (Int, Int) = {
      val picture = state.iterator.flatMap(_.iterator.map(_.sign)).mkString
      history.get(picture) match {
        case Some(day) =>
          (day, now)
        case None =>
          transform(t)
          detectLoop(t, now + 1, history + (picture -> now))
      }
    }
  }

  val Rules: Transformer = {
    case (OpenGround, count) if count(Trees) >= 3 => Trees
    case (Trees, count) if count(Lumberyard) >= 3 => Lumberyard
    case (Lumberyard, count) if count(Lumberyard) == 0 || count(Trees) == 0 => OpenGround
  }

  def toTerrain(lines: Iterable[String]): Terrain = new Terrain(
    lines.map(_.map {
      case OpenGround.sign => OpenGround
      case Trees.sign => Trees
      case Lumberyard.sign => Lumberyard
      case other => sys.error(s"Unknown symbol: `$other`")
    }).toSeq
  )

  locally {
    val test = toTerrain(
      """
        |.#.#...|#.
        |.....#|##|
        |.|..|...#.
        |..|#.....#
        |#.#|||#|#|
        |...#.||...
        |.|....|...
        |||...#|.#|
        ||.||||..|.
        |...#.|..|.
      """.trim.stripMargin.lines.toSeq
    )
    (1 to 10).foreach(_ => test.transform(Rules))
    test.count(Trees) shouldBe 37
    test.count(Lumberyard) shouldBe 31
    test.detectLoop(Rules) shouldBe (8,9)
  }

  override type Input = Terrain

  override def input: Input = toTerrain(inputLines)

  override def answer1: Answer = { t =>
    (1 to 10).foreach(_ => t.transform(Rules))
    t.count(Trees) * t.count(Lumberyard)
  }

  override def answer2: Answer = { t =>
    val (start, next) = t.detectLoop(Rules)
    val target = 1000000000
    val rest = (target - next) % (next - start)
    (1 to rest).foreach(_ => t.transform(Rules))
    t.count(Trees) * t.count(Lumberyard)
  }
}
