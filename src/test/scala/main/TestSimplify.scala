package main

import library._
import org.junit.Assert._
import org.junit.Test
import simplifier.Leothaud_Rodet_TP5._
import scala.util.Random

class TestSimplify {
  def generateRandomGlob() : String = {
    val random = new Random
    val n = random.nextInt(100)
    val alphabet = (('a' to 'z') union ('A' to 'Z') union ('0' to '9')).toSeq ++ Seq('.')
    var res = ""
    for (i <- 1 to n)
    {
      val rd = random.nextInt(4)
      rd match {
        case 0 => res +="*"
        case 1 => res += "+"
        case 2 => res += "?"
        case _ => res += alphabet(random.nextInt(alphabet.length))
      }
    }
    res
  }

  def countMinCharInGlob(glob : List[Symbol]) : Int = {
    def aux(glob : List[Symbol], acc : Int) : Int = {
      glob match {
        case Nil => acc
        case Star :: next => aux(next, acc)
        case hd :: next => aux(next, acc+1)
      }
    }
    aux(glob, 0)
  }

  def launchTestEqual(input : String ,result : String){
    val simp = new MySimplifier
    val pIn = Parser.parseSymbolList(input)
    val pRes = Parser.parseSymbolList(result)


    assertEquals(pRes, simp.simplify(pIn))
  }

  def launchTestNotEqual(input : String ,result : String){
    val simp = new MySimplifier
    val pIn = Parser.parseSymbolList(input)
    val pRes = Parser.parseSymbolList(result)


    assertNotEquals(pRes, simp.simplify(pIn))
  }

  @Test
  def unit_1() {
    launchTestEqual("a", "a")
  }


  @Test
  def complex_1() {
    launchTestEqual("******************aaa*bbb****", "*aaa*bbb*")
  }

  @Test
  def complex_2() {
    launchTestNotEqual("**", "?")
  }

  @Test
  def complex_3() {
    launchTestEqual("***fgzfg*dg*ababc*????abb+af**+++", "*fgzfg*dg*ababc+???abb+af+++")
  }

  @Test
  def complexe_4() {
    launchTestEqual("?************************************************", "+")
  }

  @Test
  def complexe_5() {
    launchTestEqual("?+*?*?*?*?*?*?*?*?*?*?*?*?*?*?*?*", "?++++++++++++++++")
  }

  @Test
  def random_test() {
    for (i <- 0 to 10000)
    {
      val simp = new MySimplifier
      val s = generateRandomGlob()
      val p = Parser.parseSymbolList(s)
      val pres = simp.simplify(p)
      assertEquals(countMinCharInGlob(p),countMinCharInGlob(pres) )
    }
  }
}