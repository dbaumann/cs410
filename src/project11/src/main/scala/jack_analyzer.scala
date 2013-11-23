package compiler

import java.io.{File, PrintWriter, FileWriter}
import scala.collection.mutable.LinkedList

/** An iterator over files that result from using `compiler` on every .jack file in `path`.
  */
class JackAnalyzer(path: String, compiler: CompilerComponent) extends Iterator[File] {

  private val inputPathFile = new File(path)
  private val inputFilesIterator = isolateJackFiles.iterator

  private def isolateJackFiles = {
    var result = new LinkedList[File]()

    val allFiles = if(inputPathFile.isFile) LinkedList(inputPathFile).toArray[File]
                   else inputPathFile.listFiles

    val jackFileName = """\w+\.jack$""".r

    allFiles.foreach {
      file => {
        file.getName match {
          case jackFileName() => result :+= file
          case _ => //ignore the file
        }
      }
    }

    result
  }

  //Iterator implementation
  def hasNext = inputFilesIterator.hasNext
  def next = {
    val nextFile = inputFilesIterator.next
    val outputPath = if(inputPathFile.isFile) inputPathFile.getParent
                     else nextFile.getParent

    val outputFileName = nextFile.getName.split("\\.").init.mkString(".") + ".mine.xml"

    //prepare the output file
    val outputFilePath = outputPath + "/" + outputFileName
    val file = new File(outputFilePath)
    if(!file.createNewFile) {
      file.delete
      file.createNewFile
    }

    //write the compiler output to the file
    val output = compiler.compile(new JackTokenizer(nextFile))
    val writer = new PrintWriter(file)
    writer.print(output)
    writer.close

    file
  }
}