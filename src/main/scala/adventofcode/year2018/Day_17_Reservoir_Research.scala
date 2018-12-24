package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec
import scala.language.postfixOps

case object Day_17_Reservoir_Research extends Puzzle {

  type Pos = (Int, Int)

  implicit class PosOps(val self: Pos) extends AnyVal {
    def x: Int = self._1
    def y: Int = self._2
  }

  case class Line(points: Iterable[Pos]) extends Iterable[Pos] {
    override def iterator: Iterator[Pos] = points.iterator
  }
  object Line {

    def apply(x: Int, ys: Range): Line = new Line(ys.map(y => (x, y)))
    def apply(xs: Range, y: Int): Line = new Line(xs.map(x => (x, y)))

    private val Pattern = """(x|y)=(\d+), (x|y)=(\d+)\.\.(\d+)""".r
    def fromString(line: String): Line = line match {
      case Pattern("x", x, "y", yMin, yMax) => Line(x.toInt, yMin.toInt to yMax.toInt)
      case Pattern("y", y, "x", xMin, xMax) => Line(xMin.toInt to xMax.toInt, y.toInt)
      case other => sys.error(s"Invalid line: `$other`")
    }
  }

  class Simulation(lines: Seq[Line]) {
    def clay: Iterator[Pos] = lines.iterator.flatMap(_.iterator)
    val left: Int = clay.map(_.x).min - 1
    val right: Int = clay.map(_.x).max + 1
    val top: Int = clay.map(_.y).min - 1
    val bottom: Int = clay.map(_.y).max + 1

    sealed abstract class State(symbol: Char, val solid: Boolean) {
      override def toString: String = symbol.toString
    }

    case object Clay extends State('#', true)
    case object Flow extends State('|', false)
    case object InfiniteFlow extends State('!', false)
    case object Water extends State('~', true)
    case object Sand extends State('.', false)

    def fill(c: State, line: Iterable[Pos]): Unit =
      volume ++= line.map(_ -> c)

    def fill(c: State, point: Pos): Unit =
      volume += point -> c

    private var volume: Map[Pos, State] = clay.map(_ -> Clay).toMap withDefaultValue Sand

    def pourAt(pos: Pos): this.type = {
      val p = (pos.x, pos.y max top)
      if (! volume.contains(p)) fill(Flow, p)
      this
    }

    def simulate(pos: Pos): Unit = volume(pos) match {
      case Flow =>
        val (x,y) = pos
        if (y >= bottom || x < left || x > right) fill(InfiniteFlow, pos)
        else volume((x, y + 1)) match {
          case InfiniteFlow =>
            val lineTop = (y to top by -1).find(y => volume((x, y - 1)) != Flow).get
            fill(InfiniteFlow, Line(x, lineTop to y + 1))
          case Sand =>
            val lineBottom = (y to bottom).find(y => volume((x,y + 1)).solid).getOrElse(bottom)
            fill(if (lineBottom == bottom) InfiniteFlow else Flow, Line(x, y to lineBottom))
          case Flow =>
          case Water|Clay =>
            def go(bound: Int, inc: Int) = (x to bound by inc)
              .find(x => volume((x + inc,y)).solid || !volume((x, y + 1)).solid).get
            val range = go(left, -1) to go(right, +1)
            val holes = range.filter(x => !volume((x, y + 1)).solid)
            val line = Line(range, y)
            if (holes.isEmpty) fill(Water, line)
            else if (holes.forall(x => volume((x, y + 1)) == InfiniteFlow)) fill(InfiniteFlow, line)
            else fill(Flow, line)
        }
      case _ => ()
    }

    def simulateStep(): Boolean = volume.toSeq.filter(_._2 == Flow).map(_._1) match {
      case Nil => false
      case flows =>
        flows.sortBy(-_.y).foreach(simulate)
        true
    }

    @tailrec
    final def run(limit: Int = 1000): this.type =
      if (simulateStep()) {
        if (limit > 0) run(limit - 1)
        else {
          debug()
          sys.error(s"Limit exceeded")
        }
      }
      else this

    def countWater: Int = volume.count {
      case (_, Water) => true
      case _ => false
    }

    def countWet: Int = volume.count {
      case (_, Clay) | (_, Sand) => false
      case ((_, y), _) if y <= top || y >= bottom => false
      case _ => true
    }

    def debug(): Unit = {
      for (y <- top to bottom) {
        Debug((left to right).map(x => volume((x, y))).mkString(" "))
      }
      Debug("")
    }
  }

  locally {
    val test =
      """
        |x=495, y=2..7
        |y=7, x=495..501
        |x=501, y=3..7
        |x=498, y=2..4
        |x=506, y=1..2
        |x=498, y=10..13
        |x=504, y=10..13
        |y=13, x=498..504
      """.trim.stripMargin.lines.toVector map Line.fromString

    val simulation = new Simulation(test)
    simulation.pourAt((500, 0))
    simulation.run()
    simulation.countWet shouldBe 57
    simulation.countWater shouldBe 29
  }

  locally {
    val test =
      """
        |x=480, y=2..10
        |x=506, y=4..11
        |y=10, x=480..508
        |x=490, y=6..8
        |x=501, y=6..8
        |y=8, x=490..501
      """.trim.stripMargin.lines.toVector map Line.fromString

    val simulation = new Simulation(test)
    simulation.pourAt((500, 0))
    simulation.run()
    simulation.countWet shouldBe 172
  }

  override type Input = Seq[Line]

  override def input: Input = inputLines.map(Line.fromString).toSeq

  override def answer1: Answer = { lines =>
    val s = new Simulation(lines).pourAt((500, 0)).run()
    s"Wet: ${s.countWet}, water: ${s.countWater}"
  }

  override def answer2: Answer = ???
}
