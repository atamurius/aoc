package adventofcode.year2018

import java.lang.Character.toUpperCase

import adventofcode.common.Puzzle

import scala.annotation.tailrec
import scala.collection.JavaConverters._

case object Day_5_Alchemical_Reduction extends Puzzle {

  override type Input = String

  override def input: Input = inputLines.mkString

  class Polymer(cs: java.util.Collection[Char]) {

    private val parts = new java.util.LinkedList[Char](cs)

    def this(s: String) = this(s.toSeq.asJava)

    def size: Int = parts.size()

    def copy(): Polymer = new Polymer(parts)

    def isReactiveAt(i: Int): Boolean = {
      if (i < size && i > 0) {
        val iterator = parts.listIterator(i - 1)
        val a, b = iterator.next()
        a != b && toUpperCase(a) == toUpperCase(b)
      }
      else false
    }
    def reactAt(i: Int): this.type = {
      val it = parts.listIterator(i - 1)
      it.next()
      it.remove()
      it.next()
      it.remove()
      this
    }

    def except(c: Char): this.type = {
      val it = parts.iterator()
      while (it.hasNext) if (toUpperCase(it.next()) == c) it.remove()
      this
    }

    override def equals(obj: Any): Boolean = obj match {
      case p: Polymer => p.parts == parts
      case _ => false
    }
  }

  new Polymer("baAB") isReactiveAt 2 shouldBe true
  new Polymer("baAB") reactAt 2 shouldBe new Polymer("bB")

  @tailrec
  def reactFully(cs: Polymer, i: Int = 1): cs.type =
    if (cs isReactiveAt i) reactFully(cs reactAt i, i - 1)
    else if (i < cs.size) reactFully(cs, i + 1)
    else (1 until cs.size).find(cs.isReactiveAt) match {
      case None => cs
      case Some(newI) => reactFully(cs, newI)
    }

  reactFully(new Polymer("dabAcCaCBAcCcaDA")) shouldBe new Polymer("dabCBAcaDA")

  override def answer1: Answer = s => reactFully(new Polymer(s)).size

  override def answer2: Answer = { s =>
    val unique = s.toUpperCase.toSet[Char]
    val minimised = reactFully(new Polymer(s))
    unique.map { problem =>
      reactFully(minimised.copy() except problem).size
    }.min
  }
}
