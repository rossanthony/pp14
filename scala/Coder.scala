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
	var keyword : String = "Pennsylvania"
	var alphabet : String = "abcdefghiklmnopqrstuvwxyz" // excluding 'j'
	  
	//var Grid = scala.collection.mutable.Map[(Int, Int), Char]()
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
	
	def writeFile() {
		val file = new File("output.txt")
		val writer = new PrintWriter(file)
		writer write "first\r\n"
		writer write "second"
		writer.close()
		val lines = Source.fromFile(file).getLines().toList
		println(lines)
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
//    println(grid(0,0)+' '+grid(0,1)+' '+grid(0,2)+' '+grid(0,3)+' '+grid(0,4))
//    println(grid(1,0))
//    println(grid(2,0))
//    println(grid(3,0))
//    println(grid(4,0))
  }
  
  /**
   * The main encoder 
   * 
   * run whenever the user enters "encode"
   * 
   * @return File: encoded
   */
	def encode() = {
	  var keyword = retrieveKeyword()
	  println(keyword)
	  
	  var test = renderGrid(keyword)
	  displayGrid(test)
	  
	  var fileContents = retrieveFile()
	}
	  
	def decode() = {
	  
	  retrieveKeyword()
	  retrieveFile()
	}
	
}