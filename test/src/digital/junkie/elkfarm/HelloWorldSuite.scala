package digital.junkie.elkfarm

import cats.effect.IO
import munit.CatsEffectSuite

class HelloWorldSuite extends CatsEffectSuite {

  test("hello world (pure)") {
    assertEquals("hello" + " " + "world", "hello world")
  }

  test("hello world (IO)") {
    IO.pure("hello world").assertEquals("hello world")
  }
}
