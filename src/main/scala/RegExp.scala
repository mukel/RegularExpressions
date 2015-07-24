
trait RE {
  def derive(c: Char): RE
  def acceptsEmpty: Boolean

  def matches(s: String): Boolean = {
    val t = (this /: s)(_ derive _)
    t.acceptsEmpty
  }
}

case object Epsilon extends RE {
  override def derive(c: Char): RE = Epsilon
  override def acceptsEmpty = true
}

case object EmptySet extends RE {
  override def derive(c: Char): RE = Epsilon
  override def acceptsEmpty = false
}

case class Union(r: RE, s: RE) extends RE {
  override def derive(c: Char): RE = Union(r.derive(c), s.derive(c))
  override def acceptsEmpty = r.acceptsEmpty || s.acceptsEmpty
}

case class Concat(r: RE, s: RE) extends RE {
  override def derive(c: Char): RE = {
    if (r.acceptsEmpty)
      Union(Concat(r.derive(c), s), s.derive(c))
    else
      Concat(r.derive(c), s)
  }
  override def acceptsEmpty: Boolean = r.acceptsEmpty && s.acceptsEmpty
}

case class Kleene(r: RE) extends RE {
  override def derive(c: Char): RE = Concat(r.derive(c), this)
  override def acceptsEmpty = true
}

case class Literal(s: String) extends RE {
  override def derive(c: Char): RE = if (s.isEmpty || c != s) EmptySet else Literal(s.tail)
  override def acceptsEmpty = s.isEmpty
}

case class SymbolRange(start: Char, end: Char) extends RE {
  override def derive(c: Char): RE = if (start <= c && c <= end) Epsilon else EmptySet
  override def acceptsEmpty = false
}

object Implicits {
  implicit def string2re(s: String): RE = Literal(s)

  implicit def rePimps(r: RE) = new {
    def | (s: RE): RE = Union(r, s)
    def ~ (s: RE): RE = Concat(r, s)
    def ^ (n: Int): RE = if (n == 0) Epsilon else Concat(r, ^(n - 1))
    def * : RE = Kleene(r)
    def + : RE = Concat(r, Kleene(r))
    def ? : RE = Union(Epsilon, r)
  }

  implicit def charPimps(start: Char) = new {
    def range(end: Char): RE = SymbolRange(start, end)
  }

  implicit def stringPimps(r: String) = new {
    def | (s: RE): RE = Union(r, s)
    def ~ (s: RE): RE = Concat(r, s)
    def ^ (n: Int): RE = if (n == 0) Epsilon else Concat(r, ^(n - 1))
    def * : RE = Kleene(r)
    def + : RE = Concat(r, Kleene(r))
    def ? : RE = Union(Epsilon, r)
  }
}

object Main {
  import Implicits._

  def main(args: Array[String]): Unit = {
    val lowerCase: RE = 'a' range 'z'
    val upperCase: RE = 'A' range 'Z'

    val digit: RE = '0' range '9'
    val nonZeroDigit: RE = '1' range '9'

    val unsigned: RE = "0" | (nonZeroDigit ~ digit.+)
    val integer: RE = ("-" | "+").? ~ unsigned

    val alphaNum = lowerCase | upperCase | digit

    println( integer matches "123913" )
    println( integer matches "0" )
    println( integer matches "" )
    println( integer matches "-12" )
    println( integer matches "+13" )

  }
}