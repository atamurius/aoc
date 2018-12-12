package adventofcode.year2018

import adventofcode.common.Puzzle

case object Day_2_Inventory_Management_System extends Puzzle {

  override type Input = Iterable[String]

  override def input: Input = inputLines

  def counts[T](xs: Iterable[T]): Map[T, Int] = xs.groupBy(identity).transform((_, vs) => vs.size)

  override def answer1: Answer = _
    .map { word =>
      val cs = counts(word).values.toSet
      Seq(2, 3).map { c =>
        if (cs contains c) 1
        else 0
      }
    }
    .foldLeft(Seq(0, 0)) { (acc, row) =>
      for ((x, y) <- acc zip row) yield x + y
    }.product

  case class Trie[T](next: Map[T, Trie[T]] = Map.empty[T, Trie[T]]) {
    def put(xs: Iterator[T]): Trie[T] =
      if (xs.hasNext) {
        val x = xs.next()
        val node = next.getOrElse(x, Trie.empty[T]) put xs
        copy(next = next + (x -> node))
      }
      else this

    def search(xs: List[T], placeholder: T, failures: Int): Iterable[List[T]] = xs match {
      case Nil => Seq(Nil)
      case _ if next.isEmpty || failures < 0 => Nil
      case x :: rest =>
        val full = next.get(x)
          .map(_.search(rest, placeholder, failures).map(x :: _))
          .getOrElse(Nil)
        val corrected = for {
          (key, next) <- this.next if key != x
          suffix <- next.search(rest, placeholder, failures - 1)
        } yield placeholder :: suffix
        full ++ corrected
      }
  }

  object Trie {
    private val _empty = Trie[Nothing]()

    def empty[T]: Trie[T] = _empty.asInstanceOf[Trie[T]]

    def apply[T](xs: Iterable[Iterable[T]]): Trie[T] = xs.foldLeft(empty[T])(_ put _.iterator)

    def debug(trie: Trie[_], indent: String = ""): Unit =
      trie.next.foreach { case (key, next) =>
        println(s"$indent$key")
        debug(next, s"$indent-")
      }
  }

  def wordTrie(ss: String*): Trie[Char] = Trie(ss.map(_.toSeq))

  locally {
    val t = wordTrie(
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"
    )
    t.search("wvxyz".toList, '_', 1) shouldBe Seq("wvxyz".toList)
    t.search("fghij".toList, '_', 1).toSet shouldBe Set("fghij".toList, "fg_ij".toList)
  }

  override def answer2: Answer = { ws =>
    val dict = wordTrie(ws.toSeq: _*)
    ws.iterator
      .map(w => dict.search(w.toList, '_', 1))
      .find(_.size > 1)
      .map(_.filter(_ contains '_').head.filter(_ != '_').mkString)
      .get
  }
}
