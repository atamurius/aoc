package adventofcode.common

abstract class Color(color: Int*) extends (Any => Unit) {
  def all(msg: Any*): Unit = println(msg.map(m => wrap(m.toString)).mkString)

  override def apply(msg: Any): Unit = println(wrap(msg.toString))

  def wrap(msg: String) = s"\u001b[${color mkString ";"}m${msg}\u001b[0m"
}

object Color {
  object Bold extends Color(1)
  object Blue extends Color(34)
  object Navy extends Color(36)
  object Gold extends Color(33, 1)
  object Green extends Color(32)
  object RedBold extends Color(31, 1)
  object Red extends Color(31)
}