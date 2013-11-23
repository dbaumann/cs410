package assembler

import java.io._

object Application {
  def main(args: Array[String]) {
    val validAsm = """^([\w/]+)\.asm$""".r

    if (args.length != 1) displayUsage()
    else args(0) match {
      case validAsm(basepath) if new File(basepath + ".asm").exists => {
        //the single argument must have an .asm file extension and exist in the file system
        var reader = new BufferedReader(new FileReader(basepath + ".asm"))
        val hackLines = Parser.processFile(reader)
        reader.close
        
        createHackFile(basepath, hackLines)
      }
      case _ => displayUsage()
    }
  }

  def displayUsage() {
    println("Usage: application.scala <filename>.asm")
  }

  def createHackFile(filePath: String, lines: Iterator[String]) {
    val parts = filePath.split("/")

    //generate parent directories
    if(parts.length > 1) new File(parts.init.mkString("/")).mkdirs()

    //write to the file
    var writer = new PrintWriter(new FileWriter(filePath + ".hack"))
    lines.foreach(writer.println(_))
    writer.close
  }
}