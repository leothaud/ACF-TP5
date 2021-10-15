package main

import library._
import org.junit.Assert._
import org.junit.Test
import simplifier.<LE_NOM_DE_VOTRE_BINOME>._

class TestSimplify {
  @Test
  def t0(){
    val simp= new MySimplifier
    val p= List(Star)
    val pres= List(Star)
    
    assertEquals(pres, simp.simplify(p))
  }
}