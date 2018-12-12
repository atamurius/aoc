package adventofcode.year2018

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import adventofcode.common.Puzzle

import scala.annotation.tailrec

case object Day_4_Repose_Record extends Puzzle {

  override type Input = Iterable[Record]

  sealed trait Record {
    def time: LocalDateTime
  }

  case class ShiftStart(time: LocalDateTime, id: Int) extends Record
  case class SleepStart(time: LocalDateTime) extends Record
  case class SleepEnd(time: LocalDateTime) extends Record

  implicit val ordering: Ordering[LocalDateTime] = Ordering.fromLessThan[LocalDateTime](_ isBefore _)

  object Record {
    private val TimeFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm")
    private val ShiftStartFormat = """\[(.*)\] Guard #(\d+) begins shift""".r
    private val SleepStartFormat = """\[(.*)\] falls asleep""".r
    private val SleepEndFormat = """\[(.*)\] wakes up""".r

    def parseTime(str: String): LocalDateTime = LocalDateTime.parse(str, TimeFormat)

    def fromString(str: String): Record = str match {
      case ShiftStartFormat(time, id) => ShiftStart(parseTime(time), id.toInt)
      case SleepEndFormat(time) => SleepEnd(parseTime(time))
      case SleepStartFormat(time) => SleepStart(parseTime(time))
    }
  }

  case class Interval(from: Int, until: Int) {
    def length: Int = until - from
    def contains(m: Int): Boolean = from <= m && m < until
  }
  implicit class IntervalOps(from: Int) {
    def -->(until: Int): Interval = Interval(from, until)
  }

  override def input: Iterable[Record] = inputLines map Record.fromString

  @tailrec
  def buildSleeps(
    sorted: List[Record],
    ints: Map[Int, List[Seq[Interval]]] = Map.empty
  ): Map[Int, List[Seq[Interval]]] = sorted match {
    case Nil => ints
    case ShiftStart(_, id) :: rest =>
      val (actions, next) = rest.span(!_.isInstanceOf[ShiftStart])
      if (actions.isEmpty) buildSleeps(next, ints)
      else {
        val intervals = actions.grouped(2).toList.map {
          case Seq(SleepStart(start), SleepEnd(end)) => Interval(start.getMinute, end.getMinute)
        }
        buildSleeps(next, ints + (id -> (intervals :: ints.getOrElse(id, Nil))))
      }
  }

  def best1(sleeps: Map[Int, List[Seq[Interval]]]): (Int, Int) = {
    val (longest, ints) = sleeps.maxBy(_._2.flatten.map(_.length).sum)
    val bestMinute = ints.flatten.map(_.from).maxBy(minute => ints.count(_.exists(_ contains minute)))
    (longest, bestMinute)
  }

  def best2(sleeps: Map[Int, List[Seq[Interval]]]): (Int, (Int, Int)) = {
    sleeps.transform { (_, intervals) =>
      intervals.flatten.map(_.from).map { minute =>
        minute -> intervals.count(_.exists(_ contains minute))
      }.maxBy(_._2)
    }.maxBy(_._2._2)
  }

  locally {
    val sleeps = buildSleeps(
      """[1518-11-01 00:00] Guard #10 begins shift
        |[1518-11-01 00:05] falls asleep
        |[1518-11-01 00:25] wakes up
        |[1518-11-01 00:30] falls asleep
        |[1518-11-01 00:55] wakes up
        |[1518-11-01 23:58] Guard #99 begins shift
        |[1518-11-02 00:40] falls asleep
        |[1518-11-02 00:50] wakes up
        |[1518-11-03 00:05] Guard #10 begins shift
        |[1518-11-03 00:24] falls asleep
        |[1518-11-03 00:29] wakes up
        |[1518-11-04 00:02] Guard #99 begins shift
        |[1518-11-04 00:36] falls asleep
        |[1518-11-04 00:46] wakes up
        |[1518-11-05 00:03] Guard #99 begins shift
        |[1518-11-05 00:45] falls asleep
        |[1518-11-05 00:55] wakes up""".stripMargin
        .lines.toList map Record.fromString
    )
    sleeps shouldBe Map(
      10 -> (Seq(24 --> 29) :: Seq(5 --> 25, 30 --> 55) :: Nil),
      99 -> (Seq(45 --> 55) :: Seq(36 --> 46) :: Seq(40 --> 50) :: Nil)
    )
    best1(sleeps) shouldBe (10, 24)
    best2(sleeps) shouldBe (99, (45, 3))
  }

  def bestBy[T](best: Map[Int, List[Seq[Interval]]] => T)(res: T => Int): Answer = { rs =>
    val timeline = rs.toList.sortBy(_.time)
    val intervals = buildSleeps(timeline)
    res(best(intervals))
  }

  override def answer1: Answer = bestBy(best1) {
    case (id, minute) => id * minute
  }

  override def answer2: Answer = bestBy(best2) {
    case (id, (minute, _)) => id * minute
  }
}
