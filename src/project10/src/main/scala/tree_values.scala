package compiler

/** A value that can be stored in a syntax tree.
  */
trait TreeValue

/** A terminal grammar element found in a syntax tree.
  */
trait Token extends TreeValue
case class KeywordToken(lexeme: String) extends Token
case class SymbolToken(lexeme: Char) extends Token
case class IntegerToken(lexeme: Int) extends Token
case class StringToken(lexeme: String) extends Token
case class IdentifierToken(lexeme: String) extends Token

/** A non-terminal grammar element found in a syntax tree.
  */
case class Declaration(name: String) extends TreeValue