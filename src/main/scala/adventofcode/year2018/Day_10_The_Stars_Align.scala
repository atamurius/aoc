package adventofcode.year2018

import adventofcode.common.Puzzle

case object Day_10_The_Stars_Align extends Puzzle {

  case class V(x: Int, y: Int) {
    def + (v: V): V = V(x = x + v.x, y = y + v.y)
  }
  case class Point(position: V, velocity: V)

  private val PointFormat = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".r

  private def toPoints(lines: Iterator[String]) = lines.filter(_.trim.nonEmpty).map {
    case PointFormat(x, y, dx, dy) => Point(V(x.toInt, y.toInt), V(dx.toInt, dy.toInt))
  }.toSeq.groupBy(_.position).transform((_, vs) => vs.map(_.velocity).toList)

  override type Input = Map[V, List[V]]

  override def input: Input = toPoints(inputLines.iterator)

  def draw(points: Map[V, Seq[V]]): Unit = {
    val minX = points.keys.map(_.x).min
    val minY = points.keys.map(_.y).min
    val maxX = points.keys.map(_.x).max
    val maxY = points.keys.map(_.y).max

    Disabled("-" * (maxX - minX + 1))
    for (y <- minY to maxY) {
      val line = for (x <- minX to maxX) yield points.getOrElse(V(x, y), Nil).size
      Disabled(line.map {
        case 0 => " "
        case n => "#"
      }.mkString)
    }
    Disabled("-" * (maxX - minX + 1))
  }

  private def tick(points: Input): Input = points.foldLeft[Input](Map.empty) {
    case (map, (pos, vs)) => vs.foldLeft(map) { (m, v) =>
      val nextPos = pos + v
      m + (nextPos -> (v :: m.getOrElse(nextPos, Nil)))
    }
  }

  def score(points: Input): Double = {
    val deltas = Seq(V(0, 1), V(1, 0), V(0, -1), V(-1, 0))
    val siblings = for(pos <- points.keys.toVector)
      yield deltas.iterator.map(_ + pos).count(points.contains)
    siblings.count(c => c > 1 && c < 4).toDouble / points.size
  }

  def findBest(points: Input): (Input, Int) =
    Iterator.iterate(points)(tick).zipWithIndex.find(ps => score(ps._1) > .7).get

  locally {
    val points = toPoints(
      """
        |position=< 9,  1> velocity=< 0,  2>
        |position=< 7,  0> velocity=<-1,  0>
        |position=< 3, -2> velocity=<-1,  1>
        |position=< 6, 10> velocity=<-2, -1>
        |position=< 2, -4> velocity=< 2,  2>
        |position=<-6, 10> velocity=< 2, -2>
        |position=< 1,  8> velocity=< 1, -1>
        |position=< 1,  7> velocity=< 1,  0>
        |position=<-3, 11> velocity=< 1, -2>
        |position=< 7,  6> velocity=<-1, -1>
        |position=<-2,  3> velocity=< 1,  0>
        |position=<-4,  3> velocity=< 2,  0>
        |position=<10, -3> velocity=<-1,  1>
        |position=< 5, 11> velocity=< 1, -2>
        |position=< 4,  7> velocity=< 0, -1>
        |position=< 8, -2> velocity=< 0,  1>
        |position=<15,  0> velocity=<-2,  0>
        |position=< 1,  6> velocity=< 1,  0>
        |position=< 8,  9> velocity=< 0, -1>
        |position=< 3,  3> velocity=<-1,  1>
        |position=< 0,  5> velocity=< 0, -1>
        |position=<-2,  2> velocity=< 2,  0>
        |position=< 5, -2> velocity=< 1,  2>
        |position=< 1,  4> velocity=< 2,  1>
        |position=<-2,  7> velocity=< 2, -2>
        |position=< 3,  6> velocity=<-1, -1>
        |position=< 5,  0> velocity=< 1,  0>
        |position=<-6,  0> velocity=< 2,  0>
        |position=< 5,  9> velocity=< 1, -2>
        |position=<14,  7> velocity=<-2,  0>
        |position=<-3,  6> velocity=< 2, -1>
      """.stripMargin.lines)

    val (best, i) = findBest(points)
    f"${score(best)}%.2f, $i" shouldBe "0.74, 3"
  }

  override def answer1: Answer = { ps =>
    val (best, i) = findBest(ps)
    draw(best)
    i
  }

  override def answer2: Answer = ??? // same as answer 1
}
