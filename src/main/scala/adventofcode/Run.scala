package adventofcode

import java.io.File
import java.net.URLClassLoader
import java.util.zip.ZipFile

import adventofcode.common.Color

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.StdIn
import scala.util.Try

object Run {

  def pathToClass: String => String = _ stripPrefix "/" stripSuffix ".class" replaceAllLiterally ("/", ".")

  def validClass: String => Boolean = _ endsWith ".class"

  def filesInFolder(f: File, base: File): Seq[String] =
    if (f.isFile) Seq(f.getAbsolutePath stripPrefix base.getAbsolutePath)
    else f.listFiles().flatMap(filesInFolder(_, base))

  def filesInJar(f: File): Seq[String] = Try {
    new ZipFile(f).entries.asScala.map(_.getName).toSeq
  }.getOrElse(Nil)

  def listClasses: Seq[String] = getClass.getClassLoader.asInstanceOf[URLClassLoader]
    .getURLs
    .filter(url => url.getProtocol == "file")
    .map(url => new File(url.getPath))
    .flatMap(f => if (f.isDirectory) filesInFolder(f, f) else filesInJar(f))
    .filter(validClass)
    .map(pathToClass)

  private val PuzzleName = """adventofcode\.year(\d+)\.Day_(\d+)_([^$]*)""".r

  case class PuzzleClass(year: Int, day: Int, name: String, cls: String)

  def parsePuzzle: PartialFunction[String, PuzzleClass] = {
    case cls @ PuzzleName(year, day, name) =>
      PuzzleClass(year.toInt, day.toInt, name.replaceAllLiterally("_", " "), cls)
  }

  private val YearDay = """(\d+)-(\d+)""".r

  def findDay(days: Map[Int, Seq[PuzzleClass]], yearDay: String): Option[PuzzleClass] = yearDay match {
    case YearDay(y, d) =>
      val day = d.toInt
      days.getOrElse(y.toInt, Nil).find(_.day == day)
    case _ => None
  }

  @tailrec
  def readDay(days: Map[Int, Seq[PuzzleClass]]): PuzzleClass =
    findDay(days, StdIn.readLine(Color.Navy.wrap("Enter year-day: "))) match {
      case Some(p) => p
      case None => Color.Red("Invalid year-day"); readDay(days)
    }

  def run(day: PuzzleClass): Unit =
    Class.forName(day.cls)
      .getMethod("main", classOf[Array[String]])
      .invoke(null, Array[String]())

  def main(args: Array[String]): Unit = {
    val days = listClasses
      .collect(parsePuzzle)
      .groupBy(_.year)
      .transform((_, ds) => ds.sortBy(_.day))

    val selected = args.headOption.flatMap(findDay(days, _))

    selected match {
      case None =>
        days.foreach { case (year, puzzles) =>
          Color.Bold(s"- $year -----------------------------")
          puzzles.foreach(p => Color.Gold.all(f"  ${p.day}%02d: ", Color.Green.wrap(p.name)))
          Color.Bold(s"-------------------------------------")
        }
        run(readDay(days))

      case Some(day) =>
        run(day)
    }
  }
}
