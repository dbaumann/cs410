package assembler

import scala.util.matching.Regex
import java.io.BufferedReader
import scala.collection._

object Parser {

  //inheritable typedef
  type IContainer = mutable.LinkedList[Instruction]
  type SContainer = mutable.HashMap[Symbol, Int]
  type BContainer = mutable.LinkedList[String]
  
  
  var instructionStash = new IContainer()
  var symbolStash = new SContainer()
  symbolStash ++:= Map(
    'SP     -> 0x0,
    'LCL    -> 0x1,
    'ARG    -> 0x2,
    'THIS   -> 0x3,
    'THAT   -> 0x4,
    'SCREEN -> 0x4000,
    'KBD    -> 0x6000
    )
  for(i <- 0 to 15) symbolStash update (Symbol("R"+i), i)

  def processFile(inputBuffer: BufferedReader): Iterator[String] = {
    mapFrom(inputBuffer, instructionStash, symbolStash) match {
      case (instructions, symbols) =>
        reduceFrom(instructions, symbols, symbolPointer = 15, new BContainer())
    }
  }

  private def mapFrom(inputBuffer: BufferedReader,
                      instructionStash: IContainer,
                      symbolStash: SContainer,
                      lineNumber: Int = 0): Tuple2[IContainer, SContainer] = {
    
    val line = inputBuffer.readLine
    if(line == null) return (instructionStash, symbolStash) //base case

    processRaw(line) match {
      //processRaw marked it as meaningless, so just forward the parameters:
      case None => mapFrom(inputBuffer, instructionStash, symbolStash)
      
      //it's an L instruction, so mutate symbolStash:
      case Some(LInstruction(symbol))
        //should not start with a number, and should not be in symbolStash:
        if("^[0-9]".r.findFirstIn(symbol) == None && symbolStash.get(Symbol(symbol)) == None) =>
          mapFrom(inputBuffer,
                  instructionStash,
                  symbolStash += ((Symbol(symbol), lineNumber)),
                  lineNumber
                  )
      
      //it's some other instruction, so mutate instructionStash and increment the line number:
      case Some(other: Instruction) => mapFrom(inputBuffer,
                                               instructionStash :+ other,
                                               symbolStash,
                                               lineNumber + 1
                                               )
    }
  }

  private def reduceFrom(instructionStash: IContainer,
                         symbolStash: SContainer,
                         symbolPointer: Int,
                         binaryCodes: BContainer): Iterator[String] = {
    
    instructionStash.headOption match {
      case None => binaryCodes.iterator //base case
      case Some(AInstruction(symbol)) => {
        if(symbolStash contains Symbol(symbol)) {
          //exists in the symbol table; recall the last value
          reduceFrom(instructionStash drop 1,
                     symbolStash,
                     symbolPointer,
                     binaryCodes :+ zeroPaddedUpto(16, symbolStash.get(Symbol(symbol)).get)
                     )
        } else if("^[a-zA-Z\\.]".r.findFirstIn(symbol) != None) {
          //new user defined constant; needs to be added to the symbol table
          reduceFrom(instructionStash drop 1,
                     symbolStash.updated(Symbol(symbol), symbolPointer + 1).asInstanceOf[SContainer],
                     symbolPointer + 1,
                     binaryCodes :+ zeroPaddedUpto(16, symbolPointer + 1)
                     )
        } else {
          //constant reference; just use that value
          reduceFrom(instructionStash drop 1,
                     symbolStash,
                     symbolPointer,
                     binaryCodes :+ zeroPaddedUpto(16, Integer.parseInt(symbol))
                     )
        }
      }
      case Some(CInstruction(dest, comp, jump)) => {
        reduceFrom(instructionStash drop 1,
                   symbolStash,
                   symbolPointer,
                   binaryCodes :+ "111" +
                      (if(CInstruction.MCodes contains Symbol(comp)) "1" else "0") +
                      zeroPaddedUpto(6, CInstruction.compCodes.get(Symbol(comp)).get) +
                      zeroPaddedUpto(3, CInstruction.destCodes.get(Symbol(dest)).get) +
                      zeroPaddedUpto(3, CInstruction.jumpCodes.get(Symbol(jump)).get)
                   )
      }
    }
  }


  private def processRaw(line: String): Option[Instruction] = {
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