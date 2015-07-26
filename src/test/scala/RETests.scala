import org.scalatest.FunSuite

/**
 * Created by mukel on 7/26/15.
 */

import REImplicits._

class RETests extends FunSuite {

  val lowerCase: RE = 'a' range 'z'
  val upperCase: RE = 'A' range 'Z'
  val vowels: RE = "aeiou".oneOf
  val digit: RE = '0' range '9'
  val nonZeroDigit: RE = '1' range '9'
  val unsigned: RE = "0" | (nonZeroDigit ~ digit.star)
  val integer: RE = ("-" | "+").? ~ unsigned
  val alphaNum = lowerCase | upperCase | digit

  test("EmptySet emptyness") {
    assert(!Epsilon.acceptsEmpty)
    for (c <- 'a' to 'z')
      assert(EmptySet.derive(c) == EmptySet)
  }

  test("Epsilon") {
    assert(Epsilon.acceptsEmpty)
    for (c <- 'a' to 'z')
      assert(Epsilon.derive(c) === EmptySet)
  }

  def matchAll(r: RE, words: String*): Boolean = {
    words forall (r matches _)
  }

  def matchNone(r: RE, words: String*): Boolean = {
    words forall (w => !(r matches w))
  }

  test("Simple concat") {
    val r = vowels concat digit;
    assert(matchAll(r, "a2", "e5", "o0"));
    assert(matchNone(r, "a", "z1", ""));
  }

  test("foor concat bar") {
    val r = "foo" concat "bar";
    assert(matchAll(r, "foobar"));
    assert(matchNone(r, "foo", "bar", "foobarfoobar", ""));
  }

  test("foo union bar") {
    val r = "foo" | "bar"
    assert(matchAll(r, "foo", "bar"));
    assert(matchNone(r, "foobar", "barfoo", ""));
  }

  test("vowels.star") {
    val r = vowels.star
    assert(matchAll(r, "aeiou", "", "a"));
    assert(matchNone(r, "aaax", "x", "axa"));
  }

  test("integers") {
    val r = integer
    assert(matchAll(r, "0", "123", "24129424920348172428", "-1", "+5", "-0", "+0"));
    assert(matchNone(r, "0123", "++1", "0x40", "1234x21", ""));
  }

  test("atLeastOnce") {
    val r = vowels.atLeastOnce
    assert(matchAll(r, "a", "ae", "aeiou"));
    assert(matchNone(r, "", "aaeex", "aeiuoxaeiuo"));
  }
}
