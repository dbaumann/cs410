package translator

import scala.util.matching.Regex
import scala.collection.mutable._

/** Abstraction of a virtual machine command.
  */
abstract class Command {
  protected def translate: LinkedList[String]

  /** Produces a list representation of a vm command which is valid Hack assembly.
    */
  def generateAssembly = LinkedList("//vm.generate " + this + ":") ++ translate
}
object Command {

  /** Generates a suitable Command object to represent the command contained in `line`.
    * @param line   the line of text that is interpreted
    * @param lineNumber the line number of the vm file being processed
    * @param functionScope  the name of the function scope in which the parser is operating
    * @param className  the name of the vm file being processed
    * @return `Option` of a `Command` representing the interpreted line, otherwise `None`
    */
  def generateFrom(line: String, lineNumber: String, functionScope: String, className: String):
  Option[Command] = line match {
    case ArithmeticCommand.PATTERN(cmd) => Some(ArithmeticCommand(cmd, className, lineNumber))
    case PushCommand.PATTERN(segment, index) => Some(PushCommand(segment, index, className))
    case PopCommand.PATTERN(segment, index) => Some(PopCommand(segment, index, className))
    case LabelCommand.PATTERN(identifier) => Some(LabelCommand(identifier, functionScope))
    case GotoCommand.PATTERN(cmdType, identifier) => {
      Some(GotoCommand(cmdType, identifier, functionScope))
    }
    case FunctionCommand.PATTERN(cmdType, functionName, numArgs) => {
      Some(FunctionCommand(cmdType, functionName, lineNumber, numArgs))
    }
    case ReturnCommand.PATTERN() => Some(ReturnCommand)
    case _ => None
  }
}


/** Represents commands that replace stack values with computed results.
  */
case class ArithmeticCommand(cmd: String, className: String, lineNumber: String = "")
extends Command {
  
  def translate = cmd match {
    case "add" => LinkedList("@SP", "M=M-1", "A=M",
                             "D=M",
                             "@SP", "M=M-1", "A=M",
                             "M=D+M",
                             "@SP", "M=M+1")

    case "sub" => LinkedList("@SP", "M=M-1", "A=M",
                             "D=M",
                             "@SP", "M=M-1", "A=M",
                             "M=M-D",
                             "@SP", "M=M+1")

    case "neg" => LinkedList("@SP", "M=M-1", "A=M",
                             "M=-M",
                             "@SP", "M=M+1")

    case "eq" => LinkedList("@SP", "M=M-1", "A=M",
                            "D=M",
                            "@SP", "M=M-1", "A=M",
                            "D=M-D",
                            "@eqFalse:"+className+"_"+lineNumber, "D;JNE",
                            "@SP", "A=M", "M=-1",
                            "@eqFinished:"+className+"_"+lineNumber, "0;JMP",
                            "(eqFalse"+lineNumber+")",
                            "@SP", "A=M", "M=0",
                            "(eqFinished:"+className+"_"+lineNumber+")",
                            "@SP", "M=M+1")

    case "gt" => LinkedList("@SP", "M=M-1", "A=M",
                            "D=M",
                            "@SP", "M=M-1", "A=M",
                            "D=M-D",
                            "@gtFalse:"+className+"_"+lineNumber, "D;JLE",
                            "@SP", "A=M", "M=-1",
                            "@gtFinished:"+className+"_"+lineNumber, "0;JMP",
                            "(gtFalse:"+className+"_"+lineNumber+")",
                            "@SP", "A=M", "M=0",
                            "(gtFinished:"+className+"_"+lineNumber+")",
                            "@SP", "M=M+1")

    case "lt" => LinkedList("@SP", "M=M-1", "A=M",
                            "D=M",
                            "@SP", "M=M-1", "A=M",
                            "D=M-D",
                            "@ltFalse:"+className+"_"+lineNumber, "D;JGE",
                            "@SP", "A=M", "M=-1",
                            "@ltFinished:"+className+"_"+lineNumber, "0;JMP",
                            "(ltFalse:"+className+"_"+lineNumber+")",
                            "@SP", "A=M", "M=0",
                            "(ltFinished:"+className+"_"+lineNumber+")",
                            "@SP", "M=M+1")

    case "and" => LinkedList("@SP", "M=M-1", "A=M",
                             "D=M",
                             "@SP", "M=M-1", "A=M",
                             "M=M&D",
                             "@SP", "M=M+1")

    case "or" => LinkedList("@SP", "M=M-1", "A=M",
                            "D=M",
                            "@SP", "M=M-1", "A=M",
                            "M=M|D",
                            "@SP", "M=M+1")

    case "not" => LinkedList("@SP", "M=M-1", "A=M",
                             "M=!M",
                             "@SP", "M=M+1")
  }
}
object ArithmeticCommand {
  val PATTERN = new Regex("(add|sub|neg|eq|gt|lt|and|or|not).*")
}


/** Represents commands that push values onto the stack.
  */
case class PushCommand(segment: String, index: String, className: String)
extends Command with VirtualMemory {
  
  def translate = segment match {
    case "constant" => LinkedList("@"+index, "D=A") ++ pushToStack
    case "local" => pullFromSegment("LCL", index) ++ pushToStack
    case "argument" => pullFromSegment("ARG", index) ++ pushToStack
    case "this" => pullFromSegment("THIS", index) ++ pushToStack
    case "that" => pullFromSegment("THAT", index) ++ pushToStack
    case "pointer" => pullFromSegment("POINTER", index) ++ pushToStack
    case "temp" => pullFromSegment("TEMP", index) ++ pushToStack
    case "static" => LinkedList("@"+className+"."+index, "D=M") ++ pushToStack
    case _ => LinkedList("//vm.error did not translate \"push " + segment + " " + index + "\"")
  }
}
object PushCommand {
  val PATTERN = new Regex("push +([a-z]+) +([0-9]+).*")
}


/** Represents commands that pop values off of the stack.
  */
case class PopCommand(segment: String, index: String, className: String)
extends Command with VirtualMemory {
  
  def translate = segment match {
    case "local" => popFromStack ++ pushToSegment("LCL", index)
    case "argument" => popFromStack ++ pushToSegment("ARG", index)
    case "this" => popFromStack ++ pushToSegment("THIS", index)
    case "that" => popFromStack ++ pushToSegment("THAT", index)
    case "pointer" => popFromStack ++ pushToSegment("POINTER", index)
    case "temp" => popFromStack ++ pushToSegment("TEMP", index)
    case "static" => popFromStack ++ LinkedList("@"+className+"."+index, "M=D")
    case _ => LinkedList("//vm.error did not translate \"pop " + segment + " " + index + "\"")
  }
}
object PopCommand {
  val PATTERN = new Regex("pop +([a-z]+) +([0-9]+).*")
}


/** Represents commands that label a location in instruction memory.
  */
case class LabelCommand(identifier: String, functionScope: String) extends Command {
  def translate = LinkedList("("+functionScope+"$"+identifier+")")
}
object LabelCommand {
  val VALID_IDENTIFIER = """[a-zA-Z_\.:][a-zA-Z_\.:0-9]*"""
  val PATTERN = new Regex("label +(" + VALID_IDENTIFIER + ").*")
}


/** Represents commands that cause the instruction memory address to jump.
  */
case class GotoCommand(cmdType: String, identifier: String, functionScope: String)
extends Command with VirtualMemory {
  
  def translate = cmdType match {
    case "goto" => LinkedList("@"+functionScope+"$"+identifier, "0;JMP")
    case "if-goto" => popFromStack ++ LinkedList("@"+functionScope+"$"+identifier, "D;JNE")
  }
}
object GotoCommand {
  val PATTERN = new Regex("((?:if-)?goto) +(" + LabelCommand.VALID_IDENTIFIER + ").*")
}

/** Represents commands that define function definitions and invocations.
  */
case class FunctionCommand(cmdType: String, functionName: String, lineNumber: String = "",
                           numArgs: String = "0")
extends Command with VirtualMemory {
  
  def translate = cmdType match {
    case "function" => {
      var result = LinkedList("("+functionName+")",
                              "@0", "D=A")
      for(i <- 0 until numArgs.toInt) result ++= pushToSegment("LCL", i.toString)
      result
    }
    case "call" => {
      LinkedList("@return:"+functionName+"_"+lineNumber, "D=A") ++ pushToStack ++
      LinkedList("@LCL", "D=M") ++ pushToStack ++ //push LCL
      LinkedList("@ARG", "D=M") ++ pushToStack ++ //push ARG
      LinkedList("@THIS", "D=M") ++ pushToStack ++ //push THIS
      LinkedList("@THAT", "D=M") ++ pushToStack ++ //push THAT
      LinkedList("@SP", "D=M",
                 "@"+numArgs, "D=D-A",
                 "@5", "D=D-A",
                 "@ARG", "M=D", //ARG = SP-n-5
                 "@SP", "D=M", "@LCL", "M=D", //LCL = SP
                 "@"+functionName, "0;JMP", //goto f
                 "(return:"+functionName+"_"+lineNumber+")") //(return)
    }
  }
}
object FunctionCommand {
  val PATTERN = new Regex("(function|call) +(" + LabelCommand.VALID_IDENTIFIER + ") +([0-9]+).*")
}

/** Represents a command to return to the previous function scope.
  */
case object ReturnCommand extends Command with VirtualMemory {
  val PATTERN = new Regex("return.*")

  def translate = {
    LinkedList("@LCL", "D=M",  //store LCL in D
               "@R13", "M=D", //store D in R13    //R13 = FRAME = LCL
               "@5", "D=A", //set D to 5
               "@R13", "A=M-D", "D=M", //set D to *(R13-5)
               "@R14", "M=D") ++ //store D in R14  //R14 = RET = *(FRAME-5)
    popFromStack ++ LinkedList("@ARG", "A=M", "M=D") ++ //*ARG = pop()
    LinkedList("@ARG", "D=M+1", "@SP", "M=D", //SP = ARG+1
               "@R13", "A=M-1", "D=M", "@THAT", "M=D", //THAT = *(FRAME-1)
               "@2", "D=A", "@R13", "A=M-D", "D=M", "@THIS", "M=D", //THIS = *(FRAME-2)
               "@3", "D=A", "@R13", "A=M-D", "D=M", "@ARG", "M=D", //ARG = *(FRAME-3)
               "@4", "D=A", "@R13", "A=M-D", "D=M", "@LCL", "M=D", //LCL = *(FRAME-4)
               "@R14", "A=M", "0;JMP") //goto RET
  }
}