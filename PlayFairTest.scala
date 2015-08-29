package playfairscalatest

import org.scalatest.FunSuite
import playfair.Coder

class PlayFairTest extends FunSuite {
  
    var c = new Coder("pennsylvania")  
    var table = c.createTableCipherTable()

  test("testing the createtable method") {
    val alphabet = List ('p','e','n','s','y','l','v','a','i','b','c','d','f','g','h','k','m','o','q','r','t','u','w','x','z')
    assert(table(0)(0).equals(alphabet(0)))
    assert(table(1)(3).equals(alphabet(8)))
  }
  
  test ("testing the formatTableLetters") {
    val keyword = "pennsylvania"
    assert(c.formatTableLetters(keyword).equals(List ('p','e','n','s','y','l','v','a','i','b','c','d','f','g','h','k','m','o','q','r','t','u','w','x','z')))
  }
  
  test ("testing the formatInputFile method") {
    val input = "@>@ This Is a STRING JjJjmxxt !',:"
    val keyword = "pennsylvania"
    assert(c.formatInputFile(input).equals("thisisastringiiximxqtz"))
  }
  
  test("testing findPosition method") {
    var c = new Coder("pennsylvania")
    val tableArray = c.createTableCipherTable()
    
    val ch = 'a'
    var res = c.findPosition(ch, tableArray)
    assert(res(0).equals(1) && res(1).equals(2))
  }
 
  test("testing changeCharIndex method") {                          
    //the position of the character to be changed is returned
    var eChanged = c.changeChar('e', 'a', "encode", table)
    //position of the first new character
    assert(eChanged(0).equals(1) && eChanged(1).equals(1))
    //position of the second new character
    assert(eChanged(2).equals(0) && eChanged(3).equals(2))
  }
 
  test("testing produceCipher method") {
    
    val st = "abcfagdciibdehghii"
    assert("ildgfifdgghvdyhcgg" == c.produceCipher(table, st, "encode").mkString)
  }
}