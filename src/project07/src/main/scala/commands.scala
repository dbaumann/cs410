package translator

import scala.collection.mutable._
import scala.util.matching._

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
    * @return       some command object representing the interpreted line, otherwise `None`
    */
  def generateFrom(className: String, line: String): Option[Command] = line match {
    case ArithmeticCommand.pattern(cmd) => Some(ArithmeticCommand(cmd))
    case PushCommand.pattern(segment, index) => Some(PushCommand(segment, index, className))
    case PopCommand.pattern(segment, index) => Some(PopCommand(segment, index, className))
    case _ => None
  }
}

/** Encapsulates the concern of virtual memory mapping.
  */
trait VirtualMemory {
  private val segments = Map("SP"      -> 0,
                             "LCL"     -> 1,
                             "ARG"     -> 2,
                             "THIS"    -> 3,
                             "THAT"    -> 4,
                             "POINTER" -> 3,
                             "TEMP"    -> 5
                             )

  /** Sets the A register to the address of base[offset]. Overwrites D register.
    */
  protected def setAddressFor(segment: String, index: String) = segment match {
    case "POINTER" => LinkedList("@"+(segments.get(segment).get+index.toInt))
    case "TEMP" => LinkedList("@"+(segments.get(segment).get+index.toInt))
    case _ => LinkedList("@"+segments.get(segment).get, "D=M", "@"+index, "A=D+A")
  }
}

/** Represents commands that replace stack values with computed results.
  */
case class ArithmeticCommand(cmd: String) extends Command {
  
  protected def translate = cmd match {
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
                            "M=M-D",
                            "D=0", "M=D&M", "M=!M",
                            "@SP", "M=M+1")

    case "gt" => LinkedList("@SP", "M=M-1", "A=M",
                            "D=M",
                            "@SP", "M=M-1", "A=M",
                            "D=M-D",
                            "@32767", "D=D|A",
                            "D=A-D", "D=!D",
                            "@SP", "A=M", "M=D",
                            "@SP", "M=M+1")

    case "lt" => LinkedList("@SP", "M=M-1", "A=M",
                            "D=M",
                            "@SP", "M=M-1", "A=M",
                            "D=M-D",
                            "@32767", "D=D|A",
                            "D=A-D",
                            "@SP", "A=M", "M=D",
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
object ArithmeticCommand { val pattern = new Regex("(add|sub|neg|eq|gt|lt|and|or|not)") }



/** Represents commands that push values onto the stack.
  */
case class PushCommand(segment: String, index: String, className: String)
  extends Command with VirtualMemory {
  
  protected def translate = segment match {
    case "constant" => LinkedList("@"+index, "D=A") ++ pushToStack
    case "local" => pullFromSegment("LCL", index) ++ pushToStack
    case "argument" => pullFromSegment("ARG", index) ++ pushToStack
    case "this" => pullFromSegment("THIS", index) ++ pushToStack
    case "that" => pullFromSegment("THAT", index) ++ pushToStack
    case "pointer" => pullFromSegment("POINTER", index) ++ pushToStack
    case "temp" => pullFromSegment("TEMP", index) ++ pushToStack
    case "static" => LinkedList("@"+className+"."+index, "D=M") ++ pushToStack
    case _ => LinkedList("//vm.error did not match \"push " + segment + " " + index + "\"")
  }


  /** Puts the value in name[offset] in D register
    */
  private def pullFromSegment(name: String, offset: String) = {
    setAddressFor(name, index) ++ LinkedList("D=M")
  }

  /** Puts the value in D register at the top of the stack, then increments the stack pointer
    */
  private def pushToStack = LinkedList("@SP", "A=M",
                                       "M=D",
                                       "@SP", "M=M+1")
}
object PushCommand { val pattern = new Regex("push +([a-z]+) +([0-9]+)") }


/** Represents commands that pop values off of the stack.
  */
case class PopCommand(segment: String, index: String, className: String)
  extends Command with VirtualMemory {
  
  protected def translate = segment match {
    case "local" => popFromStack ++ pushToSegment("LCL", index)
    case "argument" => popFromStack ++ pushToSegment("ARG", index)
    case "this" => popFromStack ++ pushToSegment("THIS", index)
    case "that" => popFromStack ++ pushToSegment("THAT", index)
    case "pointer" => popFromStack ++ pushToSegment("POINTER", index)
    case "temp" => popFromStack ++ pushToSegment("TEMP", index)
    case "static" => popFromStack ++ LinkedList("@"+className+"."+index, "M=D")
    case _ => LinkedList("//vm.error did not match \"pop " + segment + " " + index + "\"")
  }


  /** decrements the stack pointer, then puts the value at the top of the stack in D register
    */
  private def popFromStack = LinkedList("@SP", "M=M-1", "A=M",
                                        "D=M")

  /** Puts the value in D register value in name[offset]
    */
  private def pushToSegment(name: String, offset: String) = {
    LinkedList("@R13", "M=D") ++ //preserve the stack value in R13
    setAddressFor(name, index) ++ //retrieve the address to push to into A
    LinkedList("D=A", "@R14", "M=D", //move the address to push to into R14
               "@R13", "D=M", //move the stack value into D
               "@R14", "A=M", //move the address to push to into A
               "M=D") //finally, save the stack value into the address to push to
  } 
}
object PopCommand { val pattern = new Regex("pop +([a-z]+) +([0-9]+)") }