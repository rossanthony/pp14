/**
 * Playfair Cipher
 * 
 * @author Ross Anthony
 */

import scala.io.StdIn._
import scala.io.Source
import scala.util.{Try, Failure, Success}


object Playfair {
  // set the alphabet used for building the cipher grid
  var alphabet : String = "abcdefghiklmnopqrstuvwxyz" // excluding 'j'
  // declare a new type, consisting of an Int tuple + Char  
  type Grid = Map[(Int, Int), Char]

  /**
   * The main constructor method, just run the prompt
   */
  def main() = prompt() 
  
  /**
   * Prompt the user for action and try to run the command
   */
  def prompt() : Unit = runCommand(readLine("encode, decode, or quit? ")) 
  
  /**
   * Run the command entered by the user (if it matches a valid command)
   * @param cmd: the command entered by the user
   */
  def runCommand(cmd : String) = (cmd) match {
    case "encode" => println("encoding..."); encode()
    case "decode" => println("decoding..."); decode()
    case "quit"   => println("goodbye...") // do nothing, exit
    case unknown => println("Unknown command '" + unknown + "', please try again..."); prompt() 
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
   * Build the cipher grid 
   * 5 x 5 grid, consisting of a map of x y coordinates for each character (except for j)
   * @return Grid
   */
  def buildGrid(keyword: String) : Grid = {
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
  
  /**
   * Search the grid for a given char and return the grid coordinates
   */
  def gridSearch(x: Char, grid: Grid) : (Int, Int) = {
    var found = grid.find(_._2 == x)
    // check if found..?
    found.get._1
  }

  /**
   * Return the letter
   */
  def getChar(x: Int, y: Int, grid: Grid) = {
    // ensure the coordinates are no higher than 4
    var xx = (x % 5) 
    var yy = (y % 5)
    // when decoding we need to switch minus values for corresponding highest 
    // value on opposite side of the grid
    if(xx <= -1){ xx = 4 }
    if(yy <= -1){ yy = 4 }
    grid((xx, yy))
  }

  /**
   * Given a pair of chars it returns a pair of different chars from the grid using 
   * coords depending on the rules
   * dir = 1 or -1 (depending on whether we're encoding or decoding, respectively)
   */
  def shuffle(char1: Char, char2: Char, dir: Int, grid: Grid) = {
    // get the coordinates of each char
    val (x1, y1) = gridSearch(char1, grid)
    val (x2, y2) = gridSearch(char2, grid)
    
    if (y1 == y2) 
      List(getChar(x1 + dir, y1, grid), getChar(x2 + dir, y2, grid))
    else if (x1 == x2) 
      List(getChar(x1, y1 + dir, grid), getChar(x2, y2 + dir, grid))
    else 
      List(getChar(x1, y2, grid), getChar(x2, y1, grid))
  }
 
  def encode() = {
    var grid = buildGrid(retrieveKeyword())
    var lettersToEncode = retrieveFile().toList
    //println(lettersToEncode)
    var encodedMsg : String = doEncode(lettersToEncode, grid).mkString
    var chunks = splitString(encodedMsg)
    println("\n")
    outputEncodedMsg(chunks)
    prompt()
  }

  def decode() = {
    var grid = buildGrid(retrieveKeyword())
    var lettersToDecode = retrieveFile()
    //println(lettersToDecode)
    var decodedMsg : String = doDecode(lettersToDecode, grid)
    var chunks = splitString(decodedMsg)
    println("\n")
    outputEncodedMsg(chunks)
    prompt()
  }
  
  /**
   * Recursive func for doing the encoding
   */
  def doEncode(t: List[Char], grid: Grid) : List[Char] = t match {
    case x :: Nil => {
      doEncode(x :: List('x'), grid)
    }
    case x :: y :: xs => {
      if (x == y) 
        doEncode(x :: List('x'), grid) ++ doEncode(y :: xs, grid)
      else 
        shuffle(x, y, 1, grid) ++ doEncode(xs, grid)
    }
    case Nil => Nil
  }
 
  /**
   * Recursive func for doing the decoding
   */
  def doDecode(msg: String, grid: Grid) : String = {
    val letterPairs = msg.grouped(2).toList
    //displayGrid(grid)
    //println(letterPairs)
    letterPairs.flatMap(x => shuffle(x(0), x(1), -1, grid)).mkString
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