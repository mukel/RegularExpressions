# Regular Expressions

[![Travis CI Build Status](https://travis-ci.org/mukel/RegularExpressions.svg)](https://travis-ci.org/mukel/RegularExpressions)

[![Coverage Status](http://coveralls.io/repos/mukel/RegularExpressions/badge.svg?branch=master&service=github)](http://coveralls.io/github/mukel/RegularExpressions?branch=master)

This is a simple library to deal with regular expressions using derivatives, based on Brzozowski's 1964 paper "Derivatives of regular expressions".

## Performance Notice

This is a non-optimized implementation and is focused purely on correctness, so the raw complexity is exponential.

## Usage

It provides integration with the language, allowing the following:

```scala
  val lowerCase: RE = 'a' range 'z'
  val upperCase: RE = 'A' range 'Z'
  val vowels: RE = "aeiou".oneOf
  val digit: RE = '0' range '9'
  val nonZeroDigit: RE = '1' range '9'
  val unsigned: RE = "0" | (nonZeroDigit ~ digit.*)
  val integer: RE = ("-" | "+").? ~ unsigned
  val alphaNum = lowerCase | upperCase | digit
```
Matching is as simple as:

```scala
  integer matches "1234"
  email matches "foor@bar.baz"
```
