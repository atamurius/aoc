package adventofcode.common

import java.io.{PrintWriter, StringWriter}

import scala.concurrent.duration._
import scala.io.Source
import scala.util.control.NonFatal

trait Puzzle extends Product {

  type Input

  def inputLines: Iterable[String] = new Iterable[String] {
    override def iterator: Iterator[String] = {
      val stream = Option(Puzzle.this.getClass.getResourceAsStream(s"$productPrefix.txt")).getOrElse {
        sys.error(s"Can't open file $productPrefix.txt")
      }
      Source.fromInputStream(stream).getLines()
    }
  }

  final val Title = Color.Bold
  final val Disabled = Color.Blue
  final val Debug = Color.Navy
  final val Answer = Color.Gold
  final val Success = Color.Green
  final val Failure = Color.RedBold
  final val FailureDetails = Color.Red

  def input: Input

  final type Answer = Input => Any

  def answer1: Answer
  def answer2: Answer

  private var failures = false

  Title("-" * productPrefix.length)
  Title(productPrefix.replaceAll("_", " "))
  Title("-" * productPrefix.length)

  def main(args: Array[String]): Unit = {
    if (! failures) {
      answer("1", answer1)
      answer("2", answer2)
      Title("-" * productPrefix.length)
    }
  }

  private def answer(id: String, answer: => Answer): Unit = {
    val time = System.currentTimeMillis()
    try {
      val value = answer(input)
      val total = System.currentTimeMillis() - time
      Answer(s" ★ Answer $id: $value (${format(total)})")
    }
    catch {
      case _: NotImplementedError =>
        Disabled(s" ❓ Answer $id NOT IMPLEMENTED")
    }
  }

  private def format(millis: Long): String = {
    if (millis >= 1.second.toMillis) f"${millis.toDouble / 1.second.toMillis}%.2fs"
    else f"${millis}ms"
  }

  implicit class TestOps(value: => Any) {
    def shouldBe(expected: Any): Unit = {
      val line = new Exception().getStackTrace
        .find(_.getClassName == Puzzle.this.getClass.getCanonicalName)
        .map(s => s"$productPrefix.<init>(${s.getFileName}:${s.getLineNumber})")
        .getOrElse("UNKNOWN")
      try {
        val actual = value
        if (actual == expected) Success(s" ✔ Test passed ${Disabled wrap s"at $line"}")
        else {
          failures = true
          Failure(s" ✘ Test failed${Disabled wrap s" at $line"}:")
          FailureDetails(s" - Expected: $expected\n\t - Actual: $actual")
        }
      } catch {
        case NonFatal(e) =>
          failures = true
          val out = new StringWriter
          e.printStackTrace(new PrintWriter(out))
          Failure(s" ✘ Test failed${Disabled wrap s" at $line:"}")
          FailureDetails(s" - Expected: $expected\n\t - Actual: $out")
      }
    }
  }
}
