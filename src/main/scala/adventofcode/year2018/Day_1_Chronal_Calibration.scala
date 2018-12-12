package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec

case object Day_1_Chronal_Calibration extends Puzzle {

  override type Input = Iterable[Int]

  override def input: Input = inputLines.map(_.toInt)

  override def answer1: Answer = _.sum

  @tailrec def firstDuplicate[T](xs: Iterator[T], visited: Set[T] = Set.empty[T]): T = xs.next() match {
    case x if visited(x) => x
    case x =>
      firstDuplicate(xs, visited + x)
  }

  override def answer2: Answer = xs => {
    val numbers = xs.toVector
    firstDuplicate(Iterator.continually(numbers.iterator).flatten.scanLeft(0)(_ + _))
  }

  answer2(Seq(+1, -1)) shouldBe 0
  answer2(Seq(+3, +3, +4, -2, -4)) shouldBe 10
  answer2(Seq(-6, +3, +8, +5, -6)) shouldBe 5
  answer2(Seq(+7, +7, -2, -7, -4)) shouldBe 14
}
