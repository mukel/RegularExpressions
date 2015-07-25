
/**
 * Defines Regular Expression and simple (non-optimized) combinators
 *
 */
trait RE {
  def derive(c: Char): RE
  def acceptsEmpty: Boolean

  def matches(s: String): Boolean = {
    val t = (this /: s)(_ derive _)
    t.acceptsEmpty
  }

  /**
   * All future simplifications/optimizations (to reduce the exponential growth) should be implemented here.
   * Nice operators and integration is provided later by REImplicits.
   */

  // binary combinators
  def concat(r: RE): RE = Concat(this, r)
  def or(r: RE): RE = Union(this, r)
  def intersect(r: RE): RE = ???

  // unary combinators
  def complement: RE = ???
  def star: RE = Kleene(this)

  def atLeastOnce: RE = Concat(this, this.star)
  def atMostOnce: RE = Union(this, Epsilon)

  def atLeast(times: Int): RE =
    if (times == 0) star
    else
      Concat(this, atLeast(times - 1))

  def exactly(times: Int): RE =
    if (times == 0) Epsilon
    else
      Concat(this, exactly(times - 1))
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

case class Intersect(r: RE, s: RE) extends RE {
  override def derive(c: Char): RE = Intersect(r.derive(c), s.derive(c))
  override def acceptsEmpty = r.acceptsEmpty && s.acceptsEmpty
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

case class SymbolSet(val contains: Char => Boolean) extends RE {
  override def derive(c: Char): RE = if (contains(c)) Epsilon else EmptySet
  override def acceptsEmpty = false
}

case object AllSymbols extends SymbolSet(_ => true)

case class SymbolRange(start: Char, end: Char) extends SymbolSet(c => (start <= c && c <= end))

object REImplicits {
  implicit def string2re(s: String): RE = Literal(s)

  implicit def rePimps(r: RE) = new {
    def | (s: RE): RE = r or s
    def ~ (s: RE): RE = r concat s
    def ^ (n: Int): RE = r atLeast n
    def * : RE = r.star
    def + : RE = r.atLeastOnce
    def ? : RE = r.atMostOnce
  }

  implicit def charPimps(start: Char) = new {
    def range(end: Char): RE = SymbolRange(start, end)
  }

  implicit def stringPimps(r: String) = new {
    def | (s: RE): RE = r or s
    def ~ (s: RE): RE = r concat s
    def ^ (n: Int): RE = r atLeast n
    def * : RE = r.star
    def + : RE = r.atLeastOnce
    def ? : RE = r.atMostOnce
  }
}

object Main {
  import REImplicits._

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