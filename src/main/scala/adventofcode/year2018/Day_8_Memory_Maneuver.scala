package adventofcode.year2018

import adventofcode.common.Puzzle

case object Day_8_Memory_Maneuver extends Puzzle {

  case class Tree(
    children: Seq[Tree],
    metadata: Seq[Int]
  ) {
    lazy val value: Int =
      if (children.isEmpty) metadata.sum
      else metadata.map(i => children.lift(i - 1).map(_.value).getOrElse(0)).sum

    def totalMeta: Int = metadata.sum + children.map(_.totalMeta).sum
  }

  def toTree(it: Iterator[Int]): Tree = {
    val children, meta = it.next()
    Tree(
      children = (1 to children).map(_ => toTree(it)).toList,
      metadata = (1 to meta).map(_ => it.next()).toList
    )
  }

  locally {
    val tree =
      toTree("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2".split(" ").iterator.map(_.toInt))
    tree shouldBe
      Tree(
        Seq(
          Tree(Nil, Seq(10, 11, 12)),
          Tree(
            Seq(Tree(Nil, Seq(99))),
            Seq(2)
          )
        ),
        Seq(1, 1, 2)
      )
    tree.totalMeta shouldBe 138
    tree.value shouldBe 66
  }

  override type Input = Iterable[Int]

  override def input: Input = inputLines.head.split(" ").map(_.toInt)

  override def answer1: Answer = xs => toTree(xs.iterator).totalMeta

  override def answer2: Answer = xs => toTree(xs.iterator).value
}
