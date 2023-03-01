package example

import org.scalatest.funsuite.AnyFunSuite

class HelloTests extends AnyFunSuite {
  test("Hello should start with H") {
    // Hello, as opposed to hello
    assert("Hello".startsWith("H"))
  }
}