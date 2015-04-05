/**
 * Playfair Cipher
 * 
 * @author Ross Anthony
 */

import scala.io.StdIn._
import java.io._
import scala.io.Source
import scala.language.postfixOps


object Coder {
  // set a default keyword (used for testing)
  var keyword : String = "Pennsylvania"
  // set the alphabet used for building the cipher grid
  var alphabet : String = "abcdefghiklmnopqrstuvwxyz" // excluding 'j'
    
  // declare a new type, consisting of an Int tuple + Char  
  type Grid = Map[(Int, Int), Char]

  /**
   * The main constructor method, just run the prompt
   */
  def main() = {
    prompt()
  }
  
  /**
   * Prompt the user for action and try to run the command
   */
  def prompt() : Unit = { 
    runCommand(readLine("encode, decode, or quit? ")) 
  }
  
  /**
   * Run the command entered by the user (if it matches a valid command)
   * 
   * @param cmd: the command entered by the user
   */
  def runCommand(cmd : String) = (cmd) match {
    case "encode" => println("encoding..."); encode()
    case "decode" => println("decoding..."); decode()
    case "quit"   => println("goodbye...") // do nothing, exit
    case unknown => println("Unknown command \"" + unknown + "\" please try again..."); prompt() 
  }
  
  def retrieveKeyword() = {
    keyword = sanitise(readLine("Keyword: "))
    println(keyword)
    keyword
  }
  
  def retrieveFile(): String = {
    try { 
      Source.fromFile(readLine("Filename: ")).getLines.mkString 
    } catch {
      case e: Exception => ""
    }
  }
  
  /** 
   * Sanitise
   * 
   * only allow alpha chars (strip out all punctuation and any digits)
   * replace j with i and convert to lowercase
   * 
   * @return String: cleaned up
   */
  def sanitise(input: String) = {
    var sanitise = input.replaceAll("[^A-Za-z]", "") // remove all non-word chars
    sanitise map (char => if (char == 'j') 'i' else char)
    sanitise.toLowerCase
  }
  
  /**
   * Construct the cipher grid 
   * 
   * 5 x 5 grid, consisting of a map of x y coordinates for each character (excluding
   *     
   * @return Grid
   */
  def renderGrid(keyword: String) : Grid = {
    var gridChars = sanitise(keyword.concat(alphabet).distinct)
    (for (x<-0 to 4; y<-0 to 4) yield (x,y)).zip(gridChars).toMap
  }
  
  /**
   * Testing the grid, output to console to check it's correct
   */
  def displayGrid(grid: Grid) = {
    var output : String = ""
    for (x<-0 to 4; y<-0 to 4){
      output += grid(x,y) + " "
      if(y > 3) output += "\n"
    }
    println(output)
  }
  
  def lookup(x: Char, c: Grid) : (Int, Int) = {
    
    var found = c.find(_._2 == x)
    // check if found..?
    found.get._1
  }

  def get(x: Int, y: Int, grid: Grid) = grid((x % 5, y % 5))

  /**
   * 
   * dir = 1 or -1 (direction: depending on whether we're encoding or decoding, respectively)
   */
  def code(a: Char, b: Char, dir: Int, grid: Grid) = {
    val ((x1, y1), (x2, y2)) = (lookup(a, grid), lookup(b, grid))
    if (y1 == y2) 
      List(get(x1 + dir, y1, grid), get(x2 + dir, y2, grid))
    else if (x1 == x2) 
      List(get(x1, y1 + dir, grid), get(x2, y2 + dir, grid))
    else 
      List(get(x1, y2, grid), get(x2, y1, grid))
  }
 
  def encode() = {
    var keyword = retrieveKeyword()
    var grid = renderGrid(keyword)
    var lettersToEncode = sanitise(retrieveFile()).toList
    var encodedMsg : String = doEncode(lettersToEncode, grid).mkString
    var chunks = splitString(encodedMsg)
    println("\n")
    outputEncodedMsg(chunks)
  }

  def decode() = {
    var keyword = retrieveKeyword()
    var grid = renderGrid(keyword)
    var lettersToDecode = sanitise(retrieveFile())
    var test : String = doDecode(lettersToDecode, grid)
    println(test)
  }
 
  def doEncode(t: List[Char], c: Grid) : List[Char] = t match {
    case x :: Nil => doEncode(x :: List('x'), c)
    case x :: y :: xs => if (x == y) doEncode(x :: List('x'), c) ++ doEncode(y :: xs, c)
      else code(x, y, 1, c) ++ doEncode(xs, c)
    case Nil => Nil
  }
 
  def doDecode(t: String, c: Grid) : String = t grouped(2) flatMap (x => code (x(0), x(1), -1, c)) mkString

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

