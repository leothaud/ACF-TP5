package main

import library._
import org.junit.Assert._
import org.junit.Test
import simplifier.Leothaud_Rodet_TP5._

class TestSimplify {
  def launchTest(input : String ,result : String) {
    val simp = new MySimplifier
    val pIn = Parser.parseSymbolList(input)
    val pRes = Parser.parseSymbolList(result)
    val tmp = List('a' to 'z')

    assertEquals(pRes, simp.simplify(pIn))
  }
  @Test
  def empty() {
    launchTest("", "")
  }
  @Test
  def unitary_1() {
    launchTest("a", "a")
  }
  @Test
  def unitary_2() {
    launchTest("*", "*")
  }
  @Test
  def unitary_3() {
    launchTest("?", "?")
  }
  @Test
  def unitary_4() {
    launchTest("+", "+")
  }
  @Test
  def binary_1() {
    launchTest("aa", "aa")
  }
  @Test
  def binary_2() {
    launchTest("a*", "a*")
  }
  @Test
  def binary_3() {
    launchTest("a?", "a?")
  }
  @Test
  def binary_4() {
    launchTest("a+", "a+")
  }
  @Test
  def binary_5() {
    launchTest("*a", "*a")
  }
  @Test
  def binary_6() {
    launchTest("**", "*")
  }
  @Test
  def binary_7() {
    launchTest("*?", "+")
  }
  @Test
  def binary_8() {
    launchTest("*+", "+")
  }
  @Test
  def binary_9() {
    launchTest("?a", "?a")
  }
  @Test
  def binary_10() {
    launchTest("?*", "+")
  }
  @Test
  def binary_11() {
    launchTest("??", "??")
  }
  @Test
  def binary_12() {
    launchTest("?+", "?+")
  }
  @Test
  def binary_13() {
    launchTest("+a", "+a")
  }
  @Test
  def binary_14() {
    launchTest("+*", "+")
  }
  @Test
  def binary_15() {
    launchTest("+?", "+?")
  }
  @Test
  def binary_16() {
    launchTest("++", "++")
  }

}