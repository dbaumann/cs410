package assembler

trait Instruction

case class AInstruction(symbol: String) extends Instruction
case class LInstruction(symbol: String) extends Instruction

case class CInstruction(dest: String, comp: String, jump: String) extends Instruction
object CInstruction {
  import scala.collection.immutable.Map

  val destCodes = Map(
    Symbol("")    -> 0x0,
    Symbol("M")   -> 0x1,
    Symbol("D")   -> 0x2,
    Symbol("MD")  -> 0x3,
    Symbol("A")   -> 0x4,
    Symbol("AM")  -> 0x5,
    Symbol("AD")  -> 0x6,
    Symbol("AMD") -> 0x7
    )

  val ACodes = Map(
    Symbol("0")   -> 0x2a,
    Symbol("1")   -> 0x3f,
    Symbol("-1")  -> 0x3a,
    Symbol("D")   -> 0x0c,
    Symbol("A")   -> 0x30,
    Symbol("!D")  -> 0x0d,
    Symbol("!A")  -> 0x31,
    Symbol("-D")  -> 0x0f,
    Symbol("-A")  -> 0x32,
    Symbol("D+1") -> 0x1f,
    Symbol("A+1") -> 0x37,
    Symbol("D-1") -> 0x0e,
    Symbol("A-1") -> 0x32,
    Symbol("D+A") -> 0x02,
    Symbol("D-A") -> 0x13,
    Symbol("A-D") -> 0x07,
    Symbol("D&A") -> 0x00,
    Symbol("D|A") -> 0x15
    )

  val MCodes = Map(
    Symbol("M")   -> 0x30,
    Symbol("!M")  -> 0x31,
    Symbol("-M")  -> 0x32,
    Symbol("M+1") -> 0x37,
    Symbol("M-1") -> 0x32,
    Symbol("D+M") -> 0x02,
    Symbol("D-M") -> 0x13,
    Symbol("M-D") -> 0x07,
    Symbol("D&M") -> 0x00,
    Symbol("D|M") -> 0x15
    )

  val compCodes = ACodes ++ MCodes

  val jumpCodes = Map(
    Symbol("")    -> 0x0,
    Symbol("JGT") -> 0x1,
    Symbol("JEQ") -> 0x2,
    Symbol("JGE") -> 0x3,
    Symbol("JLT") -> 0x4,
    Symbol("JNE") -> 0x5,
    Symbol("JLE") -> 0x6,
    Symbol("JMP") -> 0x7
    )
}