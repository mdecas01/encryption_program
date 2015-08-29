package playfair

import scala.io._
import java.io._
import javax.swing.JFileChooser
import java.io.File
import javax.swing._
import javax.swing.filechooser._

  object Playfair {
    def main(args: Array[String]) {
      var quit = "false"
      while (quit == "false") {
        val result = readLine("Please type one option: | encode | decode | quit: ")
        result match {
        case "encode" => val input = readLine("enter key:")
                         val c = new Coder(input)
                         val encodeOutput = c.encode(c.createTableCipherTable(), c.selectFile())  
                         println("ENCRYPTED MESSAGE: ")
                         outputLineOf10(outputBlocksOf5(encodeOutput.mkString))  
                         
        case "decode" => val input = readLine("enter key:")
                         val c = new Coder(input)
                         val decodeOutput = c.decode(c.createTableCipherTable(), c.selectFile()) 
                         println("DECRYPTED MESSAGE:")
                         outputLineOf10(outputBlocksOf5(decodeOutput.mkString))
        case "quit" => quit = "true"
                       println("shut down")
                       
        case _ => println("ERROR: process not recognazed.")           
                  println("Please enter one of the commands listed above. Comands are CASE SENSITIVE.")
    }
  }
  } 
  
  /*Creates blocks containing maximum 5 characters 
   * 
   * @param text String that will be formatted 
   */
  def outputBlocksOf5(text: String): List[String] = {    
    text.length match {
      case n if n <= 5 => List(text)
      case _ => text.substring(0, 5) :: outputBlocksOf5(text.substring(5))
    }
  }
    
  /*Limits to 10 the number of blocks in each line of the output 
   * 
   * @param input String that will be formatted
   */
  def outputLineOf10(input: List[String]) {             
    if (input.length <= 10)
      println(input.mkString(" "))
    else {
      val (first, last) = input.splitAt(10)
      println(first.mkString(" "))
      outputLineOf10(last)
    }
  }
  
}

