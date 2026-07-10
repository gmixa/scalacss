package scalacss.internal

import japgolly.microlibs.testutil.TestUtil._
import utest.{TestSuite, Tests, test}
import utest.{assertThrows => assertThrows2}

object CondTest extends TestSuite {

  override def tests = Tests {
    test("pseudo") - {
      import Pseudo._
      test("not") - {
        assertEq(Not("div").cssValue, ":not(div)")
        assertEq(Not(Link).cssValue, ":not(:link)")
      }

      test("attrSelectors") - {
        assertEq(AttrExists("custom-attr").cssValue, "[custom-attr]")
        assertEq(Attr("custom-attr", "bla").cssValue, "[custom-attr=\"bla\"]")
        assertEq(AttrContains("custom-attr", "bla").cssValue, "[custom-attr~=\"bla\"]")
        assertEq(AttrStartsWith("custom-attr", "bla").cssValue, "[custom-attr|=\"bla\"]")
        assertEq(AttrEndsWith("custom-attr", "bla").cssValue, "[custom-attr$=\"bla\"]")
      }

      test("elementClassOrder") - {
        assertEq(Before.&(Hover).attrExists("custom-attr").cssValue, "[custom-attr]:hover::before")
        assertEq(Hover.attr("custom-attr", "bla").&(Before).cssValue, "[custom-attr=\"bla\"]:hover::before")
        assertEq(AttrEndsWith("custom-attr", "bla").&(Hover).&(Before).cssValue, "[custom-attr$=\"bla\"]:hover::before")
        assertEq(Before.&(Hover).nthChild("2n+1").cssValue, ":hover:nth-child(2n+1)::before")
        assertEq(Before.&(Hover).nthChild("n+4").cssValue, ":hover:nth-child(n+4)::before")
        assertEq(Before.&(Hover).nthChild("n").cssValue, ":hover:nth-child(n)::before")
        assertEq(Before.&(Hover).nthChild("4").cssValue, ":hover:nth-child(4)::before")
        assertEq(Before.&(Hover).nthChild("odd").cssValue, ":hover:nth-child(odd)::before")
        assertEq(Before.&(Hover).nthChild("even").cssValue, ":hover:nth-child(even)::before")
        assertEq(Hover.&(Before).cssValue, ":hover::before")
      }

      test("brokenNthChildQueries") - {
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("2n+k").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("2x+1").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("2n+-1").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("--2n+1").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("2n+1+3").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("2-n+1").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("2*n+1").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("2n*1").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("n/3").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("1+4").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("nn").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("-").cssValue }
        assertThrows2[IllegalArgumentException] { Before.&(Hover).nthChild("+").cssValue }
      }
    }
  }
}
