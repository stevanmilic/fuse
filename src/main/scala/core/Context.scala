package core

object Context {
  type Context = List[(String, Binding)]

  val empty = List[(String, Binding)]()

  def addBinding(c: Context, i: String, b: Binding): Context =
    (i, b) :: c

  def addName(c: Context, i: String): Context =
    (i, NameBind) :: c

  def isNameBound(c: Context, x: String): Boolean =
    c.exists { case ((i, _)) => i == x }

  def indexToName(c: Context, x: Int): Option[String] =
    c.lift(x).map { case ((i, _)) => i }

  def nameToIndex(c: Context, x: String): Option[Int] =
    c.indexWhere { case ((i, _)) => i == i } match {
      case v if v >= 0 => Some(v)
      case _           => None
    }

}
