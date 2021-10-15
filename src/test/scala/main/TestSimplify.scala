package main

import library._
import org.junit.Assert._
import org.junit.Test
import simplifier.Leothaud_Rodet_TP5._
import scala.util.Random

class TestSimplify {

  def generateRandomGlob() : String = {
    // Generate a random Glob of a random length between 0 and 99.

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
        case _ => res += alphabet(random.nextInt(alphabet.length)) // Select a random character from the alphabet
      }
    }
    res
  }

  def countMinCharInGlob(glob : List[Symbol]) : Int = {
    // Count the minimal number of characters in any string satisfying the glob.

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
    // Compute a test where the simplification of input's glob has to be exactly the result's glob.

    val simp = new MySimplifier
    val pIn = Parser.parseSymbolList(input)
    val pRes = Parser.parseSymbolList(result)
    val tmp = List('a' to 'z')

    assertEquals(pRes, simp.simplify(pIn))
  }

  def launchTestNotEqual(input : String ,result : String){
    // Same as launchTestEqual but both have to be unequals.

    val simp = new MySimplifier
    val pIn = Parser.parseSymbolList(input)
    val pRes = Parser.parseSymbolList(result)

    assertNotEquals(pRes, simp.simplify(pIn))
  }

  // Test for empty entry
  @Test
  def empty() {
    launchTestEqual("", "")
  }

  // Tests for every unitary entries possible
  @Test
  def unitary_1() {
    launchTestEqual("a", "a")
  }
  @Test
  def unitary_2() {
    launchTestEqual("*", "*")
  }
  @Test
  def unitary_3() {
    launchTestEqual("?", "?")
  }
  @Test
  def unitary_4() {
    launchTestEqual("+", "+")
  }

  // Tests for every binary entries possible
  @Test
  def binary_1() {
    launchTestEqual("aa", "aa")
  }
  def binary_1b() {
    launchTestEqual("ab", "ab") // check that the order or characters is conserved.
  }
  @Test
  def binary_2() {
    launchTestEqual("a*", "a*")
  }
  @Test
  def binary_3() {
    launchTestEqual("a?", "a?")
  }
  @Test
  def binary_4() {
    launchTestEqual("a+", "a+")
  }
  @Test
  def binary_5() {
    launchTestEqual("*a", "*a")
  }
  @Test
  def binary_6() {
    launchTestEqual("**", "*")
  }
  @Test
  def binary_7() {
    launchTestEqual("*?", "+")
  }
  @Test
  def binary_8() {
    launchTestEqual("*+", "+")
  }
  @Test
  def binary_9() {
    launchTestEqual("?a", "?a")
  }
  @Test
  def binary_10() {
    launchTestEqual("?*", "+")
  }
  @Test
  def binary_11() {
    launchTestEqual("??", "??")
  }
  @Test
  def binary_12() {
    launchTestEqual("?+", "?+")
  }
  @Test
  def binary_13() {
    launchTestEqual("+a", "+a")
  }
  @Test
  def binary_14() {
    launchTestEqual("+*", "+")
  }
  @Test
  def binary_15() {
    launchTestEqual("+?", "+?")
  }
  @Test
  def binary_16() {
    launchTestEqual("++", "++")
  }

  // Tests for complex entries, result are computed by hand.
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

  // Tests for random entries.
  // We don't have the expected result, so it just checks that the output as the same minimum characters as the input. (more details : countMinCharInGlob)
  @Test
  def random_test() {
    for (i <- 0 to 10000)
    {
      val simp = new MySimplifier
      val s = generateRandomGlob()
      val p = Parser.parseSymbolList(s)
      val pres = simp.simplify(p)
      assertEquals(countMinCharInGlob(p), countMinCharInGlob(pres))
    }
  }
}