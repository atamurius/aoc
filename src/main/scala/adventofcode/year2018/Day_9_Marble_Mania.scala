package adventofcode.year2018

import adventofcode.common.Puzzle

case object Day_9_Marble_Mania extends Puzzle {

  override type Input = (Int, Int)

  private class Marble(val value: Int) {
    private var *> = this
    private var <* = this

    private def between(<* : Marble, *> : Marble): this.type = {
      this.*> = *>
      this.<* = <*
      this.*>.<* = this
      this.<*.*> = this
      this
    }

    /** Move */
    def ~> (delta: Int): Marble = {
      var m = this
      var pos = delta
      while (pos != 0) {
        pos -= 1
        m = m.*>
      }
      m
    }

    /** Move */
    def <~ (delta: Int): Marble = {
      var m = this
      var pos = delta
      while (pos != 0) {
        pos -= 1
        m = m.<*
      }
      m
    }

    /** Insert */
    def +> (value: Int): Marble = new Marble(value) between (this, *>)

    /** Insert */
    def <+ (value: Int): Marble = new Marble(value) between (<*, this)

    /** Remove */
    def #> : Marble = {
      val res = *>
      *> between (<*, *>.*>)
      this between (this, this)
      res
    }
    /** Remove */
    def <# : Marble = {
      val res = <*
      *> between (<*, *>.*>)
      this between (this, this)
      res
    }
  }

  class Marbles {

    private var current = new Marble(0)

    def debug(): Unit = {
      var p = current
      val buff = new StringBuilder
      buff append f"(${p.value}%2d)"
      p = p ~> 1
      while (p ne current) {
        buff append f" ${p.value}%2d "
        p = p ~> 1
      }
      Disabled(buff.toString)
    }

    var score = Map.empty[Int, Long]
    var currentValue = 1

    def move(player: Int): this.type = {
      if (currentValue % 23 != 0)
        current = current ~> 1 +> currentValue
      else {
        val removed = current <~ 7
        current = removed.#>
        val revenue = currentValue + removed.value
        score += player -> (score.getOrElse(player, 0L) + revenue)
      }
      currentValue += 1
//      debug()
      this
    }

    def bestScore: Long = score.valuesIterator.max

    def move(moves: Int, players: Int): this.type = {
      for (i <- 0 until moves) {
        move(i % players + 1)
      }
      this
    }
  }

  new Marbles().move(25, 9).bestScore shouldBe 32
  new Marbles().move(1618, 10).bestScore shouldBe 8317
  new Marbles().move(7999, 13).bestScore shouldBe 146373
  new Marbles().move(1104, 17).bestScore shouldBe 2764
  new Marbles().move(6111, 21).bestScore shouldBe 54718
  new Marbles().move(5807, 30).bestScore shouldBe 37305


  override def input: Input = (426, 72058)

  override def answer1: Answer = {
    case (players, moves) => new Marbles().move(moves, players).bestScore
  }

  override def answer2: Answer = {
    case (players, moves) => new Marbles().move(moves * 100, players).bestScore
  }
}
