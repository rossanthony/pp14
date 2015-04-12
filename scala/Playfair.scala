/**
 * Playfair Cipher
 * 
 * @author Ross Anthony
 */

package playfair

import playfair._
import scala.io.StdIn._
import scala.io.Source
import scala.util.{Try, Failure, Success}

object Playfair {
  /**
   * The main constructor method, just run the prompt
   */
  def main() = prompt() 

  /**
   * Prompt the user for action and try to run the command
   */
  def prompt() : Unit = runCommand(readLine("Playfair Cipher; encode, decode, or quit? ")) 
  
  /**
   * Run the command entered by the user (if it matches a valid command)
   * @param cmd: the command entered by the user
   */
  def runCommand(cmd : String) = (cmd) match {
    case "encode" => println("encoding..."); initEncode(); prompt()
    case "decode" => println("decoding..."); initDecode(); prompt()
    case "quit"   => println("goodbye...") // don't prompt, do nothing, just exit
    case unknown  => println("Unknown command '" + unknown + "', please try again..."); prompt() 
  }

  def initEncode() = {
    var coder = new Coder(retrieveKeyword())
    var encodedMsg : String = coder.encode(retrieveFile())
    var chunks = splitString(encodedMsg)
    println("\n")
    outputEncodedMsg(chunks)
  }

  def initDecode() = {
    var coder = new Coder(retrieveKeyword())
    var decodedMsg : String = coder.decode(retrieveFile())
    var chunks = splitString(decodedMsg)
    println("\n")
    outputEncodedMsg(chunks)
  }

  /**
   * Get the keyword input from the user, sanitise it and return it as a string
   */
  def retrieveKeyword() : String = {
    var keyword = sanitise(readLine("Keyword: "))
    if(keyword=="") {
      keyword = "pennsylvania" // set a default keyword if nothing is entered (used during testing)
    }
    println(keyword)
    keyword
  }
  
  /**
   * File loader, used by the retrieveFile method
   */
  def loadFile(fileName: String): Try[String] = Try {
    scala.io.Source.fromFile(fileName).getLines.mkString
  }

  /**
   * Prompt user to input a filename, try to load it and do some sanitation 
   * on error it prints an appropriate error message and recurses to re-prompt the user
   */
  def retrieveFile() : String = {
    var file = readLine("Filename: ")
    loadFile(file) match {
      case Success(filecontents) => {
        var sanitised = sanitise(filecontents)
        Option(sanitised) match {
          case Some(sanitised) if (!sanitised.isEmpty) => sanitised
          case _ => {
            println(s"File is empty or contains only invalid chars/symbols, please select another...")
            retrieveFile()
          }
        }
      }
      case Failure(e) => {
        println(s"$file not found, please try again...")
        retrieveFile()
      }
    }
  }

  /** 
   * Sanitise
   * only allow alpha chars (strip out all punctuation and any digits) replace j with i and convert to lowercase
   * @return String: sanitised
   */
  def sanitise(input: String) : String = {
    input.replaceAll("[^A-Za-z]", "").toLowerCase.replace('j','i')
  }
  
  /** 
   * Splice up the string into 5 char chunks, return as a List
   */
  def splitString(msg: String): List[String] = {
    msg.length match {
      case n if n <= 5 => List(msg)
      case _ => msg.substring(0, 5) :: splitString(msg.substring(5))
    }
  }

  /**
   * Output the message in the required format, 10 x blocks of 5 per line
   */
  def outputEncodedMsg(chunks: List[String]) {
    if (chunks.length <= 10)
      println(chunks mkString(" "))
    else {
      val (head, tail) = chunks.splitAt(10)
      println(head mkString(" "))
      outputEncodedMsg(tail)
    }
  }
}