package compiler

import scala.collection.mutable._

object VMWriter {
  private var buffer = LinkedList.empty[String]

  /** Kind of bindings that can stored in `SymbolTable`.
  */
  object SegmentKind extends Enumeration(
      "constant", "argument", "local", "static", "this", "that", "pointer", "temp") {
    import SymbolTable.VarKind

    type SegmentKind = Value
    val CONST, ARG, LOCAL, STATIC, THIS, THAT, POINTER, TEMP = Value

    def fromSymbolKind(from: VarKind.Value) = from match {
      case VarKind.STATIC => SegmentKind.STATIC
      case VarKind.FIELD => SegmentKind.THIS
      case VarKind.ARG => SegmentKind.ARG
      case VarKind.VAR => SegmentKind.LOCAL
    }
  }

  def write(addition: String) {
    buffer :+= addition
  }

  def writePush(segment: SegmentKind.Value, index: Int) {
    buffer :+= "push " + segment.toString + " " + index
  }

  def writePop(segment: SegmentKind.Value, index: Int) {
    buffer :+= "pop " + segment.toString + " " + index
  }

  def writeCall(name: String, nArgs: Int) {
    buffer :+= "call " + name + " " + nArgs
  }

  def writeFunction(name: String, nLocals: Int) {
    buffer :+= "function " + name + " " + nLocals
  }

  def writeReturn {
    buffer ++= LinkedList("push constant 0", "return")
  }

  def writeArithmetic(operator: Char) {
    val arithmeticInstruction = operator match {
      case '*' => "call Math.multiply 2"
      case '+' => "add"
    }
    buffer :+= arithmeticInstruction
  }

  def result = buffer.mkString("\n")
}