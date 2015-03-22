/**
 * Playfair Cipher
 * 
 */
import scala.io.StdIn._
import java.io._
import scala.io.Source


object Coder {
	var keyword : String = "Pennsylvania"
	
	def main() {
		prompt(readLine("encode, decode, or quit? "))
	}
	
	def prompt(arg : String) = (arg) match {
		case "encode" => println("encoding..."); encode()
		case "decode" => println("decoding..."); decode()
		case "quit" => println("goodbye...")
		case _ => println("Unknown command, please try again..."); main() 
	}
	
	def retrieveKeyword() {
		keyword = readLine("Keyword: ")
		println(keyword)
	}
	
	def retrieveFile() {
		val filename = readLine("Filename: ") 
		try { 
			Source.fromFile(filename).getLines.mkString 
		} catch {
			case ioe: FileNotFoundException => println("File not found!")
			case e: Exception => println("Unable to open file, check permissions.")
		}
//		println(lines)
//		var test = 7+3
		//return lines
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
	
	def encode() {
	  
	  retrieveKeyword()
	  var contents = retrieveFile()
	  println(contents)
	}
	
	def decode() {
	  
	  retrieveKeyword()
	  retrieveFile()
	}
	
}