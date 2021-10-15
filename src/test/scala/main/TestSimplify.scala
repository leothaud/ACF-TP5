package main

import library._
import org.junit.Assert._
import org.junit.Test
import simplifier.Leothaud_Rodet_TP5._

class TestSimplify {
  def launchTest(input : String ,result : String){
    val simp = new MySimplifier
    val pIn = Parser.parseSymbolList(input)
    val pRes = Parser.parseSymbolList(result)


    assertEquals(pRes, simp.simplify(pIn))
  }
  @Test
  def unit_1() {
    launchTest("a", "a")
  }
}