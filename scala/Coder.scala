/**
 * Playfair Cipher
 * 
 */
import scala.io.StdIn._
import java.io._
import scala.io.Source
import scala.language.postfixOps
import scala.util.matching.Regex

object Coder {
	var keyword : String = "Pennsylvania"
	
	def main() {
		prompt(readLine("encode, decode, or quit? "))
	}
	
	/**
	 * Prompt the user for action
	 */
	def prompt(arg : String) = (arg) match {
    case "encode" => println("encoding..."); encode()
		case "decode" => println("decoding..."); decode()
		case "quit"   => println("goodbye...")
		case unknown => println("Unknown command '"+unknown+"' please try again..."); main() 
	}
	
	def retrieveKeyword() = {
		keyword = readLine("Keyword: ")
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
	 *   	only allow alpha chars (strip out all punctuation and any digits)
	 *  	replace j with i and convert to lowercase
	 * @return String
	 */
  def sanitise(input: String) = {
    var sanitise = input.replaceAll("[^A-Za-z]", "") // remove all non-word chars
    sanitise map (char => if (char == 'j') 'i' else char)
    sanitise.toLowerCase
  }
  
	def encode() {
	  var keyword = retrieveKeyword()
	  var fileContents = retrieveFile()
	  println(sanitise(fileContents))
	}
	
	def decode() {
	  
	  retrieveKeyword()
	  retrieveFile()
	}
	
}