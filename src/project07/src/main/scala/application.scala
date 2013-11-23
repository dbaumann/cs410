package translator

import java.io._
import scala.collection.mutable._

object Application {
  /** Pattern that the single application argument should match.
    */
  val validInputPath = """^([\w/]+(?:\.vm)?)$""".r

  def main(args: Array[String]) {
    if (args.length != 1) displayUsage()
    else getFilePaths(args(0)).foreach { filePath =>
      val className = filePath.split("/").last.split("\\.")(0)
      var reader = new BufferedReader(new FileReader(filePath))
      var asmLines = Parser.processFile(className, reader)
      reader.close

      createAsmFile(filePath, asmLines)
    }
  }

  private def displayUsage() {
    println("Usage: application.scala <valid path>.vm")
  }

  private def getFilePaths(abstractPath: String): LinkedList[String] = {
    var result = new LinkedList[String]()

    val file = new File(abstractPath)
    if (file.isDirectory) file.listFiles.foreach { file =>
      result ++= getFilePaths(file.getPath)
    } else abstractPath match {
      case validInputPath(filePath) => result :+= filePath
      case _ => //ignore the file
    }
    result
  }

  private def createAsmFile(filePath: String, lines: Iterator[String]) {
    val parts = filePath.split("/")

    //generate parent directories
    if(parts.length > 1) new File(parts.init.mkString("/")).mkdirs()

    //write to the file
    var writer = new PrintWriter(new FileWriter(filePath.replace(".vm", ".asm")))
    lines.foreach(writer.println(_))
    writer.close
  }
}