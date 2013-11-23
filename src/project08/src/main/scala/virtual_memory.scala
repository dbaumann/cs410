package translator

import scala.collection.mutable._

/** Encapsulates the concern of virtual memory access.
  */
trait VirtualMemory {
  /** Puts the value in D register at the top of the stack, then increments the stack pointer
    */
  protected def pushToStack = LinkedList("@SP", "A=M",
                                         "M=D",
                                         "@SP", "M=M+1")

  /** decrements the stack pointer, then puts the value at the top of the stack in D register
    */
  protected def popFromStack = LinkedList("@SP", "M=M-1", "A=M",
                                          "D=M")

  /** Puts the value in name[offset] in D register
    */
  protected def pullFromSegment(name: String, offset: String) = {
    setAddressFor(name, offset) ++ LinkedList("D=M")
  }

  /** Puts the value in D register value in name[offset]
    */
  protected def pushToSegment(name: String, offset: String) = {
    LinkedList("@R13", "M=D") ++ //preserve the stack value in R13
    setAddressFor(name, offset) ++ //retrieve the address to push to into A
    LinkedList("D=A", "@R14", "M=D", //move the address to push to into R14
               "@R13", "D=M", //move the stack value into D
               "@R14", "A=M", //move the address to push to into A
               "M=D") //finally, save the stack value into the address to push to
  } 

  //POINTER and TEMP aren't really virtual, but it's useful to treat them the same as other segments
  private val segments = Map("SP"      -> 0,
                             "LCL"     -> 1,
                             "ARG"     -> 2,
                             "THIS"    -> 3,
                             "THAT"    -> 4,
                             "POINTER" -> 3,
                             "TEMP"    -> 5
                             )

  //Sets the A register to the address of base[offset]. Overwrites D register.
  private def setAddressFor(segment: String, index: String) = segment match {
    case "POINTER" => LinkedList("@"+(segments.get(segment).get+index.toInt))
    case "TEMP" => LinkedList("@"+(segments.get(segment).get+index.toInt))
    case _ => LinkedList("@"+segments.get(segment).get, "D=M", "@"+index, "A=D+A")
  }
}