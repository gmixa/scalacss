package scalacss.internal

import scala.annotation.nowarn
import scala.concurrent.duration._
import scalacss.internal.ValueT.Rules._
import scalacss.internal.ValueT._
import utest._

import scala.language.postfixOps

object ValueTTest extends TestSuite {

  val len = Length(12, LengthUnit.px)
  val %%  = Percentage(25)
  val sec = 3 seconds
  val ms  = 3 milliseconds
  import Dsl.inherit

  override def tests = Tests {

    test("valueClasses") - {
      @nowarn("cat=unused")
      def test2[C <: ValueClass](v: ValueT[C]): Unit = ()
//      def test[C <: ValueClass] = new {
//        def apply[A](v: A)(implicit ev: A => ValueT[C]): Unit = ()
//      }
//      def test[C <: ValueClass] = new {
//        def apply[A](v: A)(implicit ev: Lazy[A => ValueT[C]]): Unit = ()
//      }
//      def test[C <: ValueClass] = new {
//        def apply[From](f: From)(implicit r: From ==> C): Unit = ()
//      }

      test("Len") - {
                      test2[Len](inherit)
        assertCompileError("test2[Len](1)")
        assertCompileError("test2[Len](1.5)")
                      test2[Len](len)
        assertCompileError("test2[Len](%%)")
        assertCompileError("test2[Len](Literal.Typed.thick)")
        assertCompileError("test2[Len](Literal.Typed.dashed)")
        assertCompileError("test2[Len](Color.black)")
        assertCompileError("test2[Len](Literal.Typed.auto)")
        assertCompileError("test2[Len](Literal.rtl)")
        assertCompileError("test2[Len](sec)")
        assertCompileError("test2[Len](ms)")
        ()
      }

      test("Pct") - {
                      test2[Pct](inherit)
        assertCompileError("test2[Pct](1)")
        assertCompileError("test2[Pct](1.5)")
        assertCompileError("test2[Pct](len)")
                      test2[Pct](%%)
        assertCompileError("test2[Pct](Literal.Typed.thick)")
        assertCompileError("test2[Pct](Literal.Typed.dashed)")
        assertCompileError("test2[Pct](Color.black)")
        assertCompileError("test2[Pct](Literal.Typed.auto)")
        assertCompileError("test2[Pct](Literal.rtl)")
        assertCompileError("test2[Pct](sec)")
        assertCompileError("test2[Pct](ms)")
        ()
      }

      test("Integer") - {
                      test2[Integer](inherit)
                      test2[Integer](1)
        assertCompileError("test2[Integer](1.5)")
        assertCompileError("test2[Integer](len)")
        assertCompileError("test2[Integer](%%)")
        assertCompileError("test2[Integer](Literal.Typed.thick)")
        assertCompileError("test2[Integer](Literal.Typed.dashed)")
        assertCompileError("test2[Integer](Color.black)")
        assertCompileError("test2[Integer](Literal.Typed.auto)")
        assertCompileError("test2[Integer](Literal.rtl)")
        assertCompileError("test2[Integer](sec)")
        assertCompileError("test2[Integer](ms)")
        ()
      }

      test("Number") - {
                      test2[Number](inherit)
                      test2[Number](1)
                      test2[Number](1.5)
        assertCompileError("test2[Number](len)")
        assertCompileError("test2[Number](%%)")
        assertCompileError("test2[Number](Literal.Typed.thick)")
        assertCompileError("test2[Number](Literal.Typed.dashed)")
        assertCompileError("test2[Number](Color.black)")
        assertCompileError("test2[Number](Literal.Typed.auto)")
        assertCompileError("test2[Number](Literal.rtl)")
        assertCompileError("test2[Number](sec)")
        assertCompileError("test2[Number](ms)")
        ()
      }

      test("LenPct") - {
                      test2[LenPct](inherit)
        assertCompileError("test2[LenPct](1)")
        assertCompileError("test2[LenPct](1.5)")
                      test2[LenPct](len)
                      test2[LenPct](%%)
        assertCompileError("test2[LenPct](Literal.Typed.thick)")
        assertCompileError("test2[LenPct](Literal.Typed.dashed)")
        assertCompileError("test2[LenPct](Color.black)")
        assertCompileError("test2[LenPct](Literal.Typed.auto)")
        assertCompileError("test2[LenPct](Literal.rtl)")
        assertCompileError("test2[LenPct](sec)")
        assertCompileError("test2[LenPct](ms)")
        ()
      }

      test("LenPctAuto") - {
                      test2[LenPctAuto](inherit)
        assertCompileError("test2[LenPctAuto](1)")
        assertCompileError("test2[LenPctAuto](1.5)")
                      test2[LenPctAuto](len)
                      test2[LenPctAuto](%%)
        assertCompileError("test2[LenPctAuto](Literal.Typed.thick)")
        assertCompileError("test2[LenPctAuto](Literal.Typed.dashed)")
        assertCompileError("test2[LenPctAuto](Color.black)")
                      test2[LenPctAuto](Literal.Typed.auto)
        assertCompileError("test2[LenPctAuto](Literal.rtl)")
        assertCompileError("test2[LenPctAuto](sec)")
        assertCompileError("test2[LenPctAuto](ms)")
        ()
      }

      test("LenPctNum") - {
                      test2[LenPctNum](inherit)
                      test2[LenPctNum](1)
                      test2[LenPctNum](1.5)
                      test2[LenPctNum](len)
                      test2[LenPctNum](%%)
        assertCompileError("test2[LenPctNum](Literal.Typed.thick)")
        assertCompileError("test2[LenPctNum](Literal.Typed.dashed)")
        assertCompileError("test2[LenPctNum](Color.black)")
        assertCompileError("test2[LenPctNum](Literal.Typed.auto)")
        assertCompileError("test2[LenPctNum](Literal.rtl)")
        assertCompileError("test2[LenPctNum](sec)")
        assertCompileError("test2[LenPctNum](ms)")
        ()
      }

      test("BrWidth") - {
                      test2[BrWidth](inherit)
        assertCompileError("test2[BrWidth](1)")
        assertCompileError("test2[BrWidth](1.5)")
                      test2[BrWidth](len)
        assertCompileError("test2[BrWidth](%%)")
                      test2[BrWidth](Literal.Typed.thick)
        assertCompileError("test2[BrWidth](Literal.Typed.dashed)")
        assertCompileError("test2[BrWidth](Color.black)")
        assertCompileError("test2[BrWidth](Literal.Typed.auto)")
        assertCompileError("test2[BrWidth](Literal.rtl)")
        assertCompileError("test2[BrWidth](sec)")
        assertCompileError("test2[BrWidth](ms)")
        ()
      }

      test("BrStyle") - {
                      test2[BrStyle](inherit)
        assertCompileError("test2[BrStyle](1)")
        assertCompileError("test2[BrStyle](1.5)")
        assertCompileError("test2[BrStyle](len)")
        assertCompileError("test2[BrStyle](%%)")
        assertCompileError("test2[BrStyle](Literal.Typed.thick)")
                      test2[BrStyle](Literal.Typed.dashed)
        assertCompileError("test2[BrStyle](Color.black)")
        assertCompileError("test2[BrStyle](Literal.Typed.auto)")
        assertCompileError("test2[BrStyle](Literal.rtl)")
        assertCompileError("test2[BrStyle](sec)")
        assertCompileError("test2[BrStyle](ms)")
        ()
      }

      test("WidStyCol") - {
                      test2[WidStyCol](inherit)
        assertCompileError("test2[WidStyCol](1)")
        assertCompileError("test2[WidStyCol](1.5)")
                      test2[WidStyCol](len)
                      test2[WidStyCol](%%)
                      test2[WidStyCol](Literal.Typed.thick)
                      test2[WidStyCol](Literal.Typed.dashed)
                      test2[WidStyCol](Color.black)
        assertCompileError("test2[WidStyCol](Literal.Typed.auto)")
        assertCompileError("test2[WidStyCol](Literal.rtl)")
        assertCompileError("test2[WidStyCol](sec)")
        assertCompileError("test2[WidStyCol](ms)")
        ()
      }

      test("Time") - {
                      test2[Time](inherit)
        assertCompileError("test2[Time](1)")
        assertCompileError("test2[Time](1.5)")
        assertCompileError("test2[Time](len)")
        assertCompileError("test2[Time](%%)")
        assertCompileError("test2[Time](Literal.Typed.thick)")
        assertCompileError("test2[Time](Literal.Typed.dashed)")
        assertCompileError("test2[Time](Color.black)")
        assertCompileError("test2[Time](Literal.Typed.auto)")
        assertCompileError("test2[Time](Literal.rtl)")
                      test2[Time](sec)
                      test2[Time](ms)
        ()
      }

    }
  }
}
