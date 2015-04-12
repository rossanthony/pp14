/**
 * Playfair Cipher
 * 
 * @author Ross Anthony */

package playfair

class Coder(val keyword: String) {

  // declare a new type, consisting of an Int tuple + Char  
  type Grid = Map[(Int, Int), Char]

  // set the alphabet used for building the cipher grid
  var alphabet : String = "abcdefghiklmnopqrstuvwxyz" // excluding 'j'

  var grid : Grid

  def main() = {
    grid = buildGrid(keyword)
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
   * Return the letter in a given position within the grid
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
 
  // def encode(plainText: String) : String = {
    
  //   var lettersToEncode = retrieveFile().toList
  //   //println(lettersToEncode)
  //   var encodedMsg : String = doEncode(lettersToEncode, grid).mkString
    
    
    
    
  // }

  // def decode() = {
  //   var grid = buildGrid(keyword)
  //   var lettersToDecode = retrieveFile()
  //   //println(lettersToDecode)
  //   var decodedMsg : String = doDecode(lettersToDecode, grid)
  //   var chunks = splitString(decodedMsg)
  //   println("\n")
  //   outputEncodedMsg(chunks)
  //   prompt()
  // }
  
  /**
   * Recursive func for doing the encoding
   */
  def encode(plainText:String) : String = doEncode(plainText.toList, grid).mkString

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
  def decode(msg: String, grid: Grid) : String = {
    val letterPairs = msg.grouped(2).toList
    //displayGrid(grid)
    //println(letterPairs)
    letterPairs.flatMap(x => shuffle(x(0), x(1), -1, grid)).mkString
  }

  /** 
   * Sanitise
   * only allow alpha chars (strip out all punctuation and any digits) replace j with i and convert to lowercase
   * @return String: sanitised
   */
  def sanitise(input: String) : String = {
    input.replaceAll("[^A-Za-z]", "").toLowerCase.replace('j','i')
  }

  // /** 
  //  * Splice up the string into 5 char chunks, return as a List
  //  */
  // def splitString(msg: String): List[String] = {
  //   msg.length match {
  //     case n if n <= 5 => List(msg)
  //     case _ => msg.substring(0, 5) :: splitString(msg.substring(5))
  //   }
  // }

  // /**
  //  * Output the message in the required format, 10 x blocks of 5 per line
  //  */
  // def outputEncodedMsg(chunks: List[String]) {
  //   if (chunks.length <= 10)
  //     println(chunks mkString(" "))
  //   else {
  //     val (head, tail) = chunks.splitAt(10)
  //     println(head mkString(" "))
  //     outputEncodedMsg(tail)
  //   }
  // }
}