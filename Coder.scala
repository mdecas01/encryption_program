package playfair

import scala.io._
import java.io._
import javax.swing.JFileChooser
import java.io.File
import javax.swing._
import javax.swing.filechooser._

 /**Class responsible for the encoding and decoding processes
  * 
  * @Constructor creates a new Coder with a keyword 
  * @param keyword that will be used to create the cipher table
  */
  class Coder(keyword: String) {
    
    val key = keyword
    
    /*Performs the encryption of the plain text
     * 
     * @param encodeArr multidimensional array used for the encryption process
     * @param f String that will be transformed into cipher text 
     */
    def encode(encodeArr: Array[Array[Char]], f: String): List[Char] = {
      val cipher = produceCipher(encodeArr, f,"encode").toList       
      cipher
    }
    
    /*Performs the decryption process
     * 
     *@param decodeArr multidimentional array used for the decryption process
     * 
     */
    def decode(decodeArr: Array[Array[Char]], c: String): List[Char] = {
      val plainText = produceCipher(decodeArr, c,"decode").toList 
      plainText
    }
    /*Formats the letters that will populate the multidimensional array
     *that act as a cipher table
     * 
     * @param keyword input entered by the user
     */
    def formatTableLetters(keyword: String): List[Char] = {
      val alph = "abcdefghiklmnopqrstuvwxyz".toLowerCase()
      val tableLetters = ((keyword.toLowerCase()) + alph).distinct.toList
      tableLetters.map(x => if (x == 'j') 'i' else x)
    }
  
    /*Creates the cipher table 
     * 
     */
    def createTableCipherTable(): Array[Array[Char]] = {
      val alphabet = formatTableLetters(key)
      var table = Array.ofDim[Char](5,5)
      var i = 0
      for(j <- 0 to 4 ) {
        for(l <- 0 to 4) {
          table(j)(l) = alphabet(i)
          i = i + 1
        }
      }
      table
    }
    
    /*Displays an dialog box that will allow the user to choose a file
     * 
     * 
     */
    def selectFile(): String =  {
      val chooser = new JFileChooser("select file")
      chooser.showOpenDialog(null)
      val source = Source.fromFile(chooser.getSelectedFile)
      val lines = formatInputFile((source.getLines).mkString)
      lines
  }
    
    /*
     * Formats the string text to only lowercase letters
     * with no ponctuations and substitutes any 'j' character
     * by a 'i' character
     * 
     * @param str ..................
     */
    def formatInputFile(str: String): String = {
      val formatted = str.map(_ .toLower).filter (_.isLetter)
      var output = formatted.map(x => if(x == 'j') 'i' else x)
      //if only one element is left adds z to make a pair
      if (output.length % 2 != 0) {
        output = output :+ 'z'
      }
        for ( i <- 1 to output.length by 2) { 
          //if pair has two similar elements one is set to x
          if (output(i) == output(i - 1)) {      //the small bug that results on goxgle instead of google
            if (output(i) == 'x') {
              output = output.updated(i,'q')
            }else {
              output = output.updated(i, 'x')
            }
          } 
        }
       output
     }
    
    
    /*Finds the position of a given character in a multidimensional Array
     * 
     * @param ch Char from String 
     * @param table multidimansional array in which the position of ch will be returned 
     */
    def findPosition (ch: Char, table: Array[Array[Char]]): Array[Int] = {
    var result = Array(0,0)                
    for (i <- 0 to table.length - 1) {
      for (j <- 0 to 4) {           
        if (table(i)(j) == (ch)) {          
          result.update(0, i)
          result.update(1, j)
         }
      }
    }
    result
  }
    
    /*Finds the position of two characters in the cipher table and swaps them for 
     *other characters according to the playfair rules
     * 
     *@param ch1 Char first element form pair that will be searched in the table
     *@param ch2 Char second element form pair that will be searched in the table
     *@param mode String that will indicate the process that is being performed either encode or decode
     *@param arr multidimensional array that works as a table for the encode and decode processes
     */
    def changeChar(ch1: Char, ch2: Char, mode: String, arr: Array[Array[Char]]): Array[Int] = {
      var posChar1 = findPosition(ch1, arr)
      var posChar2 = findPosition(ch2, arr)
      var out =  new Array[Int](4)
      //if both elements are in the same 'column' move them one row forward or backward depending on the process being performed
      if (posChar1.apply(1) == posChar2.apply(1)) {
        if (mode == "encode") {
        out(0) = posChar1.apply(0) + 1
        out(1) = posChar1.apply(1)
        out(2) = posChar2.apply(0) + 1
        out(3) = posChar2.apply(1)
        }else {
        out(0) = posChar1.apply(0) - 1
        out(1) = posChar1.apply(1)
        out(2) = posChar2.apply(0) - 1
        out(3) = posChar2.apply(1)
        }
        //if both elements are in the same 'row' move them one 'column' above or below depending on the process being performed
      } else if (posChar1(0) == posChar2(0)) {
        if (mode == "encode") {
        out(0) = posChar1.apply(0)
        out(1) = posChar1.apply(1) + 1 
        out(2) = posChar2.apply(0)
        out(3) = posChar2.apply(1) + 1 
        }else{
        out(0) = posChar1.apply(0)
        out(1) = posChar1.apply(1) - 1 
        out(2) = posChar2.apply(0)
        out(3) = posChar2.apply(1) - 1
        }
        //swaps the elements forming a 'virtual square'
      } else {
        out(0) = posChar2.apply(0)
        out(1) = posChar1.apply(1)   
        out(2) = posChar1.apply(0)   
        out(3) = posChar2.apply(1)
      }
      //if is more then the 'rows' or 'columns' indexes changes for the begining of the row or column
      out.map{(x: Int) => if (x > 4) { out.update(out.indexOf(x), 0)}else if(x < 0) { out.update(out.indexOf(x), 4)}}
      out
    }
    
    /*Produces the final ciphertext
     * 
     *@param arr multidimensional array
     *@param st String...........
     *@param mode process being performed
     */
    def produceCipher(arr: Array[Array[Char]], st: String, mode: String): Array[Char] = {
      var resArr = new Array[Char](st.length)
      for (i <- 0 to st.length) {     
        if ( i % 2 != 0) {
          var in1 = st.apply(i - 1)
          var in2 = st.apply(i)
          
          var resOut = changeChar(in1,in2,mode,arr)
          var res1 = arr(resOut.apply(0))(resOut.apply(1))
          var res2 = arr(resOut.apply(2))(resOut.apply(3))
          
          resArr.update((i - 1), res1)                        
          resArr.update((i), res2)                              
        }
      }
      resArr
    }
    
    
  
  
}