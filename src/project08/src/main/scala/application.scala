package translator

import java.io._
import scala.util.matching.Regex
import scala.collection.mutable._

object Application {
  /** Pattern that the single application argument should match.
    */
  val validInputPath = """([\w/]+)([\w]+)(?:\.vm|/)?""".r

  def main(args: Array[String]) {
    if (args.length != 1) { displayUsage(); return }

    var asmLines = new LinkedList[String]()

    getBasePaths(args(0)).foreach { basePath =>
      val className = basePath.split("/").last
      var reader = new BufferedReader(new FileReader(basePath + ".vm"))
      asmLines ++= Parser.processFile(className, reader)
      reader.close
    }

    //extract parts from the specified path
    val validInputPath(rootPath, baseName) = args(0)

    createAsmFile(rootPath + baseName, asmLines.iterator)
  }

  private def displayUsage() {
    println("Usage: application.scala <valid path>.vm")
  }

  private def getBasePaths(abstractPath: String): LinkedList[String] = {
    var result = new LinkedList[String]()

    val file = new File(abstractPath)
    if (file.isDirectory) file.listFiles.foreach { file =>
      result ++= getBasePaths(file.getPath)
    } else abstractPath match {
      case validInputPath(rootPath, baseName) => result :+= rootPath + baseName
      case _ => //ignore the file
    }
    result
  }

  private def createAsmFile(basePath: String, lines: Iterator[String]) {
    val parts = basePath.split("/")
    val pathIsDir = new File(basePath).isDirectory

    //generate parent directories
    if(parts.length > 1) new File(parts.init.mkString("/")).mkdirs

    val outputFileName = if(pathIsDir) basePath + "/" + parts.last + ".asm"
                         else basePath + ".asm"

    var writer = new PrintWriter(new FileWriter(outputFileName))

    //write bootstrap assembly code, if a Sys.vm file is provided
    val sysFilePath = if(pathIsDir) basePath + "/Sys.vm"
                      else parts.init + "/Sys.vm"

    if(new File(sysFilePath).exists) {
      writer.println("//vm.bootstrap")
      vmInitialization.foreach(writer.println(_))
    }

    //write program assembly code
    writer.println("//vm.main")
    lines.foreach(writer.println(_))

    writer.close
  }

  private val vmInitialization = LinkedList("@256", "D=A", "@SP", "M=D") ++ //SP = 256
                                 FunctionCommand("call", "Sys.init").translate
}