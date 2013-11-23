package translator

import java.io.BufferedReader
import scala.collection.mutable._

/** Interface to vm command translation capabilities.
  */
object Parser {

  /** Consumes a file stream of virtual machine commands, generating assembly instructions for each
    * valid command found.
    * @param reader   file stream referring to a valid .vm file
    * @return         string iterator over assembly instructions
    */
  def processFile(className: String, reader: BufferedReader): LinkedList[String] = {
    var result = new LinkedList[String]()
    
    var line = reader.readLine
    var lineNumber = 0
    var functionScope = ""
    var command = Command.generateFrom(line, lineNumber.toString, functionScope, className)
    
    while (line != null) {
      if(command.isDefined) result ++= command.get.generateAssembly
      else if(line.length > 0 && line.charAt(0) != '/') {
        result ++= LinkedList("//vm.error did not recognize \"" + line + "\"")
      }
      
      line = reader.readLine
      lineNumber += 1

      command match {
        case Some(FunctionCommand(_, functionName, _, _)) => functionScope = functionName
        case _ => //use existing function scope, if it exists
      }

      command = Command.generateFrom(line, lineNumber.toString, functionScope, className)
    }

    result
  }
}