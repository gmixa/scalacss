package scalacss.internal

import japgolly.microlibs.testutil.TestUtil._
import nyaya.prop._
import nyaya.test.PropTest._
import utest._

object ComposeTest extends TestSuite {
  import Compose.{Rules => R}

  type TT = (StyleS, StyleS)

//  val associativity =
//    Prop.equal[TTT]("Associativity")(
//      { case (a,b,c) => x((a compose b) compose c) },
//      { case (a,b,c) => x(a compose (b compose c)) })

  def flat(s: StyleS) =
    Css.flatten4(s.copy(className = None).inspectCss)

  case class PropTest(a: StyleS, b: StyleS) {
    val E = EvalOver(this.toString.take(200))

    def appendTest = {
      implicit val impc = new Compose(R(R.append, R.silent))
      E.equal("sort(a) + sort(b) = sort(a + b)",
        (flat(a) ++ flat(b)).sorted.toVector,
        flat(a compose b).sorted.toVector)
    }

    def replaceTest = {
      implicit val impc = new Compose(R(R.replace, R.silent))
      E.allPresent("a∘b ⊇ b",
        flat(b).toSet,
        flat(a compose b))
    }

    //def all = "Props" rename_: (appendTest & replaceTest)
    def all = appendTest
  }

  val propTest =
    RandomData.styleS.pair.map((PropTest.apply _).tupled)

  object Issue25 {
    import scalacss.defaults.Exports._
    import scalacss.defaults.DefaultSettings.Dev._
    object SampleStyles extends StyleSheet.Inline {
      override implicit val classNameHint = ClassNameHint("TEST")
      import dsl._
      val other = style(borderCollapse.collapse, &.hover(fontWeight._200), fontWeight._100)
      val outer = style(fontWeight.bold)
      val inner = style(color.red, outer)
    }
    def test(): Unit = {
      val css = SampleStyles.renderA[String].trim
      assertEq(SampleStyles.outer.htmlClass, "TEST-outer")
      assertEq(SampleStyles.inner.htmlClass, "TEST-inner")
      assertMultiline(css,
        """
          |.TEST-other {
          |  border-collapse: collapse;
          |  font-weight: 100;
          |}
          |
          |.TEST-other:hover {
          |  font-weight: 200;
          |}
          |
          |.TEST-outer {
          |  font-weight: bold;
          |}
          |
          |.TEST-inner {
          |  color: red;
          |  font-weight: bold;
          |}
        """.stripMargin.trim)
    }
  }

  override def tests = Tests {
    test("props")   - propTest.mustSatisfyE(_.all) //(defaultPropSettings.setSampleSize(2000))
    test("issue25") - Issue25.test()

    test("values") - {
      import Dsl._
      implicit def c: Compose = Compose.safe

      def test2(s: StyleS)(avs: AV*)(ws: String*): Unit = {
        assertEq(s.data(Cond.empty).avIterator.toVector, avs.toVector)
        assertEq(s.warnings.map(_.msg), ws.toVector)
      }

      test("idempotency") - {
        val a = style(display.block)
        test2(a compose a)(AV(display, "block"))()
      }

      test("sameKey") -
        test2(style(display.block) compose style(display.inline))(
          AV(display, "block"), AV(display, "inline")
        )("{display: inline} conflicts with {display: block}")

      test("marginN1") -
        test2(style(margin.auto) compose style(marginLeft.`0`))(
          AV(margin, "auto"), AV(marginLeft, "0")
        )("{margin-left: 0} conflicts with {margin: auto}")

      test("margin1N") -
        test2(style(marginLeft.`0`) compose style(margin.auto))(
          AV(marginLeft, "0"), AV(margin, "auto")
        )("{margin: auto} conflicts with {margin-left: 0}")

    }
  }
}
