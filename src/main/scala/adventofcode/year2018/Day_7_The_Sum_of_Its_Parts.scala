package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.Function.const
import scala.annotation.tailrec

case object Day_7_The_Sum_of_Its_Parts extends Puzzle {

  type Order[T] = (T, T)

  private val OrderFormat = """Step (.*) must be finished before step (.*) can begin.""".r

  def toOrder(str: String): Order[String] = str match {
    case OrderFormat(a, b) => a -> b
  }

  override type Input = Iterable[Order[String]]

  override def input: Input = inputLines map toOrder

  def order[T: Ordering](
    orders: Seq[Order[T]],
    length: T => Int,
    workers: Int
  ): (Seq[T], Int) = {

    @tailrec def collect(
      pending: Seq[T],
      needs: Map[T, Set[T]],
      work: Map[T, Int] = Map.empty,
      finished: Vector[T] = Vector.empty,
      time: Int = 0
    ): (Vector[T], Int) =
      if (pending.isEmpty) (finished, time)
      else {
        val tick = if (work.isEmpty) 0 else work.valuesIterator.min
        val (done, wip) = work.transform((_, t) => t - tick).partition(_._2 <= 0)
        val doneTasks = done.keys.toSeq.sorted
        val finished2 = finished ++ doneTasks
        val pending2 = pending diff doneTasks
        val needs2 = needs -- doneTasks

        val ready = pending2.iterator.filter { p =>
          !wip.contains(p) && needs2.getOrElse(p, Set.empty).forall(finished2.contains)
        }
        val todo = ready.take(workers - wip.size).toVector
        val work2 = wip ++ todo.map(t => t -> length(t))

        collect(
          pending = pending2,
          needs = needs2,
          work = work2,
          finished = finished2,
          time = time + tick
        )
      }

    val requires = orders.groupBy(_._2).transform((_, ps) => ps.map(_._1).toSet)
    val pending = orders.foldLeft(Set.empty[T]) {
      case (set, (a, b)) => set + a + b
    }.toVector.sorted

    collect(pending, requires)
  }

  locally {

    val orders =
      """
        |Step C must be finished before step A can begin.
        |Step C must be finished before step F can begin.
        |Step A must be finished before step B can begin.
        |Step A must be finished before step D can begin.
        |Step B must be finished before step E can begin.
        |Step D must be finished before step E can begin.
        |Step F must be finished before step E can begin.
      """.stripMargin.lines.filter(_.trim.nonEmpty).toSeq map toOrder

    order(
      orders = orders,
      length = const(1),
      workers = 1
    ) shouldBe ("CABDFE".split("").toVector, 6)

    order(
      orders = orders,
      length = { name: String => name.head.toInt - 'A'.toInt + 1 },
      workers = 2
    ) shouldBe ("CABFDE".split("").toVector, 15)
  }
  override def answer1: Answer = ls =>
    order(
      orders = ls.toSeq,
      length = const(1),
      workers = 1
    )._1.mkString

  override def answer2: Answer = ls =>
    order(
      orders = ls.toSeq,
      length = { s: String => s.head.toInt - 'A'.toInt + 61 },
      workers = 5
    )._2
}
