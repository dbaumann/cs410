package compiler

import java.io.File

/** An example client of the compiler.
  */
object Main extends App {
  new JackAnalyzer(args(0), new HackCompiler()).foreach { file: File => 
    println("compiled to " + file.getName)
  }
}