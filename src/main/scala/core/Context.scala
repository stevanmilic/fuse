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

  def pickFreshName(c: Context, x: String): (Context, String) =
    isNameBound(c, x) match {
      case true  => pickFreshName(c, s"$x'")
      case false => (addName(c, x), x)
    }

  def indexToName(c: Context, x: Int): Option[String] =
    c.lift(x).map { case ((i, _)) => i }

  def nameToIndex(c: Context, x: String): Option[Int] =
    c.indexWhere { case ((i, _)) => i == x } match {
      case v if v >= 0 => Some(v)
      case _           => None
    }

  def getTypeFromContext(c: Context, idx: Int): Either[String, Type] =
    getBinding(c, idx) match {
      case Right(VarBind(t)) => Right(t)
      case Right(_) =>
        Left(
          s"getTypeFromContext: Wrong kind of binding for variable ${indexToName(c, idx)}"
        )
      case Left(e) => Left(e)
    }

  def getBinding(c: Context, idx: Int): Either[String, Binding] =
    c.lift(idx) match {
      case Some((_, b)) => Right(b)
      case _            => Left(s"Variable not found: offset $idx, ctx size ${c.length}")
    }

}
