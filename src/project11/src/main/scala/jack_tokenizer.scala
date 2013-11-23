package compiler

import java.io.{File, FileReader, BufferedReader}
import scala.util.matching.Regex

/** Patterns used during tokenization.
  */
object JackTokenizer {
  //non-tokens
  val singleLineComment = new Regex("""^(//.*)""")
  val multiLineCommentOpen = new Regex("""^(/\*.*)""")
  val multiLineCommentClose = new Regex("""^(.*\*/)""")
  val whitespace = new Regex("""^([ \t]+).*""")

  //tokens
  val keyword = new Regex("^(class|constructor|function|method|field|static|var" +
                          "|int|char|boolean|void|true|false|null|this" +
                          "|let|do|if|else|while|return).*")

  val symbol = new Regex("""^(\{|\}|\(|\)|\[|\]|\.|,|;|\+|-|\*|/|&|\||<|>|=|~).*""")

  val integer = new Regex("""^(\d+).*""")

  val string = new Regex("""^\"(.*)\".*""")

  val identifier = new Regex("""^([a-zA-Z_][\w_]*).*""")
}

/** An iterator over tokens found in a .jack file.
  */
class JackTokenizer(file: File) extends Iterator[Token] {
  private val fileReader = new BufferedReader(new FileReader(file))
  private var nextLine = fileReader.readLine
  private var nextToken = pullTokenFromFile

  private def pullTokenFromFile: Option[Token] = {
    if(nextLine.isEmpty) nextLine = fileReader.readLine
    if(nextLine == null) return None

    //filter out whitespace, comments
    consumeNonTokens

    if(nextLine == null) return None

    //determine the next token
    nextLine match {
      case JackTokenizer.keyword(matchVal) => {
        nextLine = nextLine.drop(matchVal.length)
        Some(KeywordToken(matchVal))
      }
      case JackTokenizer.symbol(matchVal) => {
        nextLine = nextLine.drop(1)
        Some(SymbolToken(matchVal.charAt(0)))
      }
      case JackTokenizer.integer(matchVal) => {
        nextLine = nextLine.drop(matchVal.length)
        Some(IntegerToken(matchVal.toInt))
      }
      case JackTokenizer.string(matchVal) => {
        nextLine = nextLine.drop(matchVal.length + 2)
        Some(StringToken(matchVal))
      }
      case JackTokenizer.identifier(matchVal) => {
        nextLine = nextLine.drop(matchVal.length)
        Some(IdentifierToken(matchVal))
      }
      case _ => {
        throw new RuntimeException("Unrecognized token found in line: " + nextLine)
      }
    }
  }

  private def consumeNonTokens {
    while(nextLine.isEmpty) nextLine = fileReader.readLine
    if(nextLine == null) return

    nextLine match {
      case JackTokenizer.singleLineComment(ignored) => {
        nextLine = fileReader.readLine
        consumeNonTokens
      }
      case JackTokenizer.multiLineCommentOpen(ignored) => {
        while(JackTokenizer.multiLineCommentClose.findFirstIn(nextLine) == None) {
          nextLine = fileReader.readLine
        }
        nextLine = nextLine.drop(ignored.length)
        consumeNonTokens
      }
      case JackTokenizer.whitespace(ignored) => {
        nextLine = nextLine.drop(ignored.length)
        consumeNonTokens
      }
      case _ => {
        //valid token has been found
        return
      }
    }
  }

  //Iterator implementation
  def hasNext = nextToken != None
  def next = {
    val retVal = nextToken.get
    nextToken = pullTokenFromFile
    retVal
  }
}