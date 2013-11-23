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
  def processFile(className: String, reader: BufferedReader): Iterator[String] = {
    var line = reader.readLine
    var command = Command.generateFrom(className, line)
    var result = new LinkedList[String]()

    while (line != null) {
      if(command.isDefined) result ++= command.get.generateAssembly
      line = reader.readLine
      command = Command.generateFrom(className, line)
    }

    result.iterator
  }
}