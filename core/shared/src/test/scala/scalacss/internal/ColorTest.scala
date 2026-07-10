package scalacss.internal

import japgolly.microlibs.testutil.TestUtil._
import scala.annotation.nowarn
import utest._

object ColorTest extends TestSuite {
  import Macros.Color
  import Dsl._

  override def tests = Tests {

    test("valid") - {
      def test2(c: Color, expect: String) =
        assertEq(c.value, expect)

      test("hex3") - test2(c"#f09"     , "#f09")
      test("hex4") - test2(c"#f0a9"    , "#f0a9")
      test("hex6") - test2(c"#abc105"  , "#abc105")
      test("hex8") - test2(c"#abc105f3", "#abc105f3")

      test("rgbI") - test2(c"rgb(0,128,255)",    "rgb(0,128,255)")
      test("rgbP") - test2(c"rgb(0%,50%,100%)",  "rgb(0%,50%,100%)")
      test("rgba") - test2(c"rgba(255,128,0,0)", "rgba(255,128,0,0)")
      test("hsl")  - test2(c"hsl(359,0%,100%)",  "hsl(359,0%,100%)")
      test("hsla") - test2(c"hsla(0,100%,0%,1)", "hsla(0,100%,0%,1)")

      test("hexU") - test2(c"#ABC",           "#abc")
      test("rgbU") - test2(c"RGB(0,128,255)", "rgb(0,128,255)")

      test("whitespace") - test(c"  rgba  (  255  ,  128  ,  0  ,  0  )  ", "rgba(255,128,0,0)")

      test("alphaDec0") - test(c"hsla(0,100%,0%,0.918)", "hsla(0,100%,0%,0.918)")
      test("alphaDecZ") - test(c"hsla(0,100%,0%,.543)",  "hsla(0,100%,0%,.543)")
    }

    test("invalid") - {
      @nowarn("cat=unused")
      def assertFailure(e: CompileError) = ()
      def assertErrorContains(e: CompileError, frag: String): Unit =
        assertContains(e.msg, frag)

      test("hex") - {
        def test2(e: CompileError): Unit = assertErrorContains(e, "Hex notation must be")
        test("0") - test2(assertCompileError(""" c"#" """))
        test("1") - test2(assertCompileError(""" c"#f" """))
        test("2") - test2(assertCompileError(""" c"#00" """))
        test("5") - test2(assertCompileError(""" c"#12345" """))
        test("7") - test2(assertCompileError(""" c"#1234567" """))
        test("9") - test2(assertCompileError(""" c"#123456789" """))
        test("g") - test2(assertCompileError(""" c"#00g" """))
        test("G") - test2(assertCompileError(""" c"#G00" """))
      }

      test("empty") - assertFailure(assertCompileError( """ c"" """))
      test("blank") - assertFailure(assertCompileError( """ c"   " """))
      test("badFn") - assertFailure(assertCompileError( """ c"rbg(0,0,0)" """))
      test("two")   - assertFailure(assertCompileError( """ c"#fed #fed" """))

      test("numbers") - {
        test("r-1")   - assertErrorContains(assertCompileError(""" c"rgb(-1,0,0)" """), "Invalid red value")
        test("r256")  - assertErrorContains(assertCompileError(""" c"rgb(256,0,0)" """), "Invalid red value")
        test("g256")  - assertErrorContains(assertCompileError(""" c"rgb(0,256,0)" """), "Invalid green value")
        test("b256")  - assertErrorContains(assertCompileError(""" c"rgb(0,0,256)" """), "Invalid blue value")
        test("a2")    - assertErrorContains(assertCompileError(""" c"rgba(0,0,0,2)" """), "Invalid alpha value")
        test("a1.1")  - assertErrorContains(assertCompileError(""" c"rgba(0,0,0,1.1)" """), "Invalid alpha value")
        test("r101%") - assertErrorContains(assertCompileError(""" c"rgb(101%,0%,0%)" """), "Invalid red value")
        test("g101%") - assertErrorContains(assertCompileError(""" c"rgb(0%,101%,0%)" """), "Invalid green value")
        test("b101%") - assertErrorContains(assertCompileError(""" c"rgb(0%,0%,101%)" """), "Invalid blue value")
        test("dbl")   - assertFailure      (assertCompileError(""" c"rgb(2.5,0,0)" """))
        test("str")   - assertFailure      (assertCompileError(""" c"rgb(x,0,0)" """))
        test("empty") - assertFailure      (assertCompileError(""" c"rgb(0,,0)" """))
        test("mixed") - assertFailure      (assertCompileError(""" c"rbg(0,0%,0)" """))
      }
    }
  }
}
