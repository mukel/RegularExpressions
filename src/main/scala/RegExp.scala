/**
 * Defines very simple Regular Expressions with basic (non-optimized) combinators.
 */
trait RE {
  def derive(c: Char): RE
  def acceptsEmpty: Boolean

  /**
   * All future simplifications/optimizations (to reduce the exponential growth) should be implemented here.
   * Nice operators and integration is provided later by REImplicits.
   */

  // Binary combinators
  def concat(r: RE): RE = Concat(this, r)
  def or(r: RE): RE = Union(this, r)
  def intersect(r: RE): RE = Intersect(this, r)

  // Unary combinators
  def complement: RE = Complement(this)
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

  def matches(s: String): Boolean = {
    val t = (this /: s)(_ derive _)
    t.acceptsEmpty
  }
}

// Empty String
case object Epsilon extends RE {
  def derive(c: Char): RE = EmptySet
  def acceptsEmpty = true
}

case object EmptySet extends RE {
  def derive(c: Char): RE = EmptySet
  def acceptsEmpty = false
}

case class Union(r: RE, s: RE) extends RE {
  def derive(c: Char): RE = Union(r.derive(c), s.derive(c))
  def acceptsEmpty = r.acceptsEmpty || s.acceptsEmpty
}

case class Intersect(r: RE, s: RE) extends RE {
  def derive(c: Char): RE = Intersect(r.derive(c), s.derive(c))
  def acceptsEmpty = r.acceptsEmpty && s.acceptsEmpty
}

case class Concat(r: RE, s: RE) extends RE {
  def derive(c: Char): RE = {
    if (r.acceptsEmpty)
      Union(Concat(r.derive(c), s), s.derive(c))
    else
      Concat(r.derive(c), s)
  }
  def acceptsEmpty: Boolean = r.acceptsEmpty && s.acceptsEmpty
}

case class Kleene(r: RE) extends RE {
  def derive(c: Char): RE = Concat(r.derive(c), this)
  def acceptsEmpty = true
}


case class Complement(r: RE) extends RE {
  def derive(c: Char): RE = Complement(r.derive(c))
  def acceptsEmpty = !r.acceptsEmpty
}

case class Symbol(s: Char) extends RE {
  def derive(c: Char): RE = if (c != s) EmptySet else Epsilon
  def acceptsEmpty = false
}

case class Literal(s: String) extends RE {
  def derive(c: Char): RE = if (s.isEmpty || c != s.head) EmptySet else Literal(s.tail)
  def acceptsEmpty = s.isEmpty
}

class SymbolSet(val contains: Char => Boolean) extends RE {
  def derive(c: Char): RE = if (contains(c)) Epsilon else EmptySet
  def acceptsEmpty = false
}

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
    def unary_! : RE = r.complement
  }

  implicit def charPimps(start: Char) = new {
    def range(end: Char): RE = SymbolRange(start, end)
  }

  implicit def charPimps2(c: Char) = Symbol(c)

  implicit def stringPimps(r: String) = new {
    def | (s: RE): RE = r or s
    def ~ (s: RE): RE = r concat s
    def ^ (n: Int): RE = r atLeast n
    def * : RE = r.star
    def + : RE = r.atLeastOnce
    def ? : RE = r.atMostOnce
    def unary_! : RE = r.complement

    // Allows something like vowels = "aeiou".oneOf
    def oneOf: RE = new SymbolSet(c => r contains c)
  }
}
