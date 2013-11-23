package assembler

import scala.util.matching._
import scala.collection._
import java.io.BufferedReader

object Parser {
  var instructions = new mutable.LinkedList[Instruction]()
  var symbolTable = mutable.HashMap(
    'SP     -> 0x0,
    'LCL    -> 0x1,
    'ARG    -> 0x2,
    'THIS   -> 0x3,
    'THAT   -> 0x4,
    'SCREEN -> 0x4000,
    'KBD    -> 0x6000
    )
  for(i <- 0 to 15) symbolTable update (Symbol("R"+i), i)
  var variableSymbolPointer = 16

  def processFile(reader: BufferedReader): Iterator[String] = {
    var line = reader.readLine()
    var lineNum = 0
    var instruction: Option[Instruction] = None

    //first pass
    while (line != null) {
      instruction = processNext(line)
      
      if (instruction.isDefined)
        instruction.get match {
          case LInstruction(symbol) if("^[0-9]".r.findFirstIn(symbol) == None) => {
            if(symbolTable.get(Symbol(symbol)) == None) {
              symbolTable update (Symbol(symbol), lineNum)
            }
          }
          case other:Instruction => {
            instructions :+= other
            lineNum += 1
          }
        }
      
      line = reader.readLine()
    }

    //second pass; every case is expected to yield a string. the collected result is returned
    for (instruction <- instructions.iterator) yield instruction match {
      case AInstruction(symbol) => {
        if(symbolTable contains Symbol(symbol)) {
          //exists in the symbol table; recall the last value
          zeroPaddedUpto(16, symbolTable.get(Symbol(symbol)).get)
        } else if("^[a-zA-Z\\.]".r.findFirstIn(symbol) != None) {
          //new user defined constant; needs to be added to the symbol table
          symbolTable update (Symbol(symbol), variableSymbolPointer)
          val binaryResult = zeroPaddedUpto(16, variableSymbolPointer)
          variableSymbolPointer += 1
          binaryResult
        } else {
          //constant reference; just use that value
          zeroPaddedUpto(16, Integer.parseInt(symbol))
        }
      }
      case CInstruction(dest, comp, jump) => {
        "111" + (if(CInstruction.MCodes contains Symbol(comp)) "1" else "0") +
              zeroPaddedUpto(6, CInstruction.compCodes.get(Symbol(comp)).get) +
              zeroPaddedUpto(3, CInstruction.destCodes.get(Symbol(dest)).get) +
              zeroPaddedUpto(3, CInstruction.jumpCodes.get(Symbol(jump)).get)
      }
    }
  }

  private def processNext(line: String): Option[Instruction] = {
    if (line.startsWith("//") || line.isEmpty) return None
    var text = """//""".r.split(line)(0).trim
    new Some(interpret(text))
  }

  private def interpret(text: String): Instruction = {
    val identifier = """[a-zA-Z_\.\$:][\w_\.\$:]*"""
    val constant = """\d+"""

    val destMnemonic = "A?M?D?"
    val compMnemonic = "[-!]?[AMD01][+\\-!&|]?[AMD01]?"
    val jumpMnemonic = "(?:JGT|JEQ|JGE|JLT|JNE|JLE|JMP)?"

    val aFormat = new Regex("^@(" + identifier + "|" + constant + ")")
    val lFormat = new Regex("^\\((" + identifier + "|" + constant + ")\\)$")
    val cFormat = new Regex("(" + destMnemonic + ")=?(" + compMnemonic + ");?(" + jumpMnemonic + ")")

    text match {
      case aFormat(symbol) => AInstruction(symbol)
      case lFormat(symbol) => LInstruction(symbol)
      case cFormat(dest, comp, jump) => CInstruction(dest, comp, jump)
    }
  }

  private def zeroPaddedUpto(totalLength: Int, intVal: Int): String = {
    val thisInstruction = Integer.toString(intVal, 2)
    "0" * (totalLength - thisInstruction.length) + thisInstruction
  }
}