package adventofcode.year2018

import adventofcode.common.Puzzle
import adventofcode.year2018.Day_19_Go_With_The_Flow.{Instr, Machine}

import scala.annotation.tailrec

case object Day_21_Chronal_Conversion extends Puzzle {

  override type Input = Machine

  override def input: Input = Instr.parseCode(inputLines.iterator) match { case (ip, code) => Machine(code, ip.reg) }

  def runTo(m: Machine, line: Int, debug: Set[Int] = Set.empty[Int]): m.type = {
    do {
      m.tick()
      if (debug(m.ip)) Debug(m.debug)
    } while (m.ip != line)
    m
  }

  override def answer1: Answer = runTo(_, 28).memory(2)

  class Simulation {
    var r2 = 0
    final def next(): Int = {
      var r5 = r2 | 65536
      r2 = 4843319
      var loop = true
      while (loop) {
        var r4 = r5 & 0xFF
        r2 = (((r2 + r4) & 0xFFFFFF) * 65899) & 0xFFFFFF
        if (256 > r5) loop = false
        else {
          r4 = -1
          var r3 = 0
          do {
            r4 += 1
            r3 = (r4 + 1) * 256
          }
          while (r3 <= r5)
          r5 = r4
        }
      }
      r2
    }

    @tailrec
    final def findLoop(visited: collection.mutable.Set[Int] = collection.mutable.Set.empty[Int], prev: Int = 0): Int = {
      val curr = next()
      if (visited(curr)) prev
      else findLoop(visited += curr, curr)
    }
  }

  locally {
    val m = input
    val expected = Iterator.continually(runTo(m, 28).memory(2))
    val s = new Simulation
    val actual = Iterator.continually(s.next())
    val N = 100
    actual.take(N).toList shouldBe expected.take(N).toList
  }

  override def answer2: Answer = { _ =>
    new Simulation().findLoop()
  }
}
