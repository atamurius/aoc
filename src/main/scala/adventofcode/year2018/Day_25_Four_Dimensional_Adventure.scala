package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec

case object Day_25_Four_Dimensional_Adventure extends Puzzle {

  type V4 = (Int, Int, Int, Int)

  def distance(a: V4, b: V4): Int =
    (a.productIterator zip b.productIterator).map { case (a: Int, b: Int) => math.abs(a - b) }.sum

  def nearOrAt(d: Int)(a: V4, b: V4): Boolean = distance(a, b) <= 3

  def toV4(line: String): V4 = line.split(",").map(_.trim.toInt) match {
    case Array(a,b,c,d) => (a,b,c,d)
  }

  @tailrec
  def colour[T](
    xs: List[T],
    near: (T, T) => Boolean,
    color: Map[T, Int] = Map.empty[T, Int],
    next: Int = 0
  ): Map[Int, Set[T]] = xs match {
    case Nil => color.groupBy(_._2).transform((_, ts) => ts.keySet)
    case x :: rest => color.keys.filter(near(x, _)).toList match {
      case Nil => colour(rest, near, color + (x -> next), next + 1)
      case y :: ys =>
        val singleColor = color(y)
        val repaintColors = ys.map(color).toSet - singleColor
        val repaintPoints = color.collect { case (p, c) if repaintColors(c) => p }.toSeq
        colour(rest, near, color ++ (x +: repaintPoints).map(_ -> singleColor), next)
    }
  }

  locally {

    colour[V4](
      """
        | 0,0,0,0
        | 3,0,0,0
        | 0,3,0,0
        | 0,0,3,0
        | 0,0,0,3
        | 0,0,0,6
        | 9,0,0,0
        |12,0,0,0
      """.trim.stripMargin.lines.map(toV4).toList,
      nearOrAt(3)
    ).size shouldBe 2

    colour[V4](
      """
        |-1,2,2,0
        |0,0,2,-2
        |0,0,0,-2
        |-1,2,0,0
        |-2,-2,-2,2
        |3,0,2,-1
        |-1,3,2,2
        |-1,0,-1,0
        |0,2,1,-2
        |3,0,0,0
      """.trim.stripMargin.lines.map(toV4).toList,
      nearOrAt(3)
    ).size shouldBe 4

    colour[V4](
      """
        |1,-1,0,1
        |2,0,-1,0
        |3,2,-1,0
        |0,0,3,1
        |0,0,-1,-1
        |2,3,-2,0
        |-2,2,0,0
        |2,-2,0,-1
        |1,-1,0,-1
        |3,2,0,2
      """.trim.stripMargin.lines.map(toV4).toList,
      nearOrAt(3)
    ).size shouldBe 3

    colour[V4](
      """
        |1,-1,-1,-2
        |-2,-2,0,1
        |0,2,1,3
        |-2,3,-2,1
        |0,2,3,-2
        |-1,-1,1,-2
        |0,-2,-1,0
        |-2,2,3,-1
        |1,2,2,0
        |-1,-2,0,-2
      """.trim.stripMargin.lines.map(toV4).toList,
      nearOrAt(3)
    ).size shouldBe 8
  }

  override type Input = List[V4]

  override def input: Input = inputLines.map(toV4).toList

  override def answer1: Answer = colour[V4](_, nearOrAt(3)).size

  override def answer2: Answer = ???
}
