package compiler

import scala.collection.mutable.LinkedList

/** A `Parser` which understands the Jack grammar.
  */
object JackParser extends Parser {

  def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
    tokenizer.head match {
      case KeywordToken("class") => {
        tree.appendLeaf(tokenizer.next) //append class
        tree.appendLeaf(tokenizer.next) //append className
        parse(tokenizer, tree)
      }
      case SymbolToken('{') => {
        tree.appendLeaf(tokenizer.next) //append opening bracket
        ClassDef.parse(tokenizer, tree)
        tree.appendLeaf(tokenizer.next) //append closing bracket
        return
      }
    }
  }

  private object ClassDef extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken(lexeme) if isClassVarDecStart(lexeme) => {
          ClassVar.parse(tokenizer, tree.appendTree(Declaration("classVarDec")))
          parse(tokenizer, tree)
        }
        case KeywordToken(lexeme) if isSubroutineStart(lexeme) => {
          Subroutine.parse(tokenizer, tree.appendTree(Declaration("subroutineDec")))
          parse(tokenizer, tree)
        }
        case SymbolToken('}') => return //appended higher up
      }
    }
  }

  private object ClassVar extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken(lexeme) if isClassVarDecStart(lexeme) => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case headToken: Token if isType(headToken) => {
          tree.appendLeaf(tokenizer.next) //append type
          tree.appendLeaf(tokenizer.next) //append varName
          parse(tokenizer, tree)
        }
        case SymbolToken(',') => {
          tree.appendLeaf(tokenizer.next) //append comma
          tree.appendLeaf(tokenizer.next) //append varName
          parse(tokenizer, tree)
        }
        case SymbolToken(';') => {
          tree.appendLeaf(tokenizer.next)
          return
        }
      }
    }
  }

  private object Subroutine extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken(lexeme) if isSubroutineStart(lexeme) => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case headToken: Token if isSubroutineReturnType(headToken) => {
          tree.appendLeaf(tokenizer.next) //append type
          tree.appendLeaf(tokenizer.next) //append subroutineName
          parse(tokenizer, tree)
        }
        case SymbolToken('(') => {
          tree.appendLeaf(tokenizer.next) //append (
          ParameterList.parse(tokenizer, tree.appendTree(Declaration("parameterList")))
          tree.appendLeaf(tokenizer.next) //append )
          parse(tokenizer, tree)
        }
        case SymbolToken('{') => {
          SubroutineBody.parse(tokenizer, tree.appendTree(Declaration("subroutineBody")))
          return
        }
      }
    }
  }

  private object ParameterList extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case headToken: Token if isType(headToken) => {
          tree.appendLeaf(tokenizer.next) //append type
          tree.appendLeaf(tokenizer.next) //append varName
          parse(tokenizer, tree)
        }
        case SymbolToken(',') => {
          tree.appendLeaf(tokenizer.next) //append comma
          parse(tokenizer, tree)
        }
        case SymbolToken(')') => return //appended higher up
      }
    }
  }

  private object SubroutineBody extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case SymbolToken('{') => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case KeywordToken("var") => {
          VariableDeclaration.parse(tokenizer, tree.appendTree(Declaration("varDec")))
          parse(tokenizer, tree)
        }
        case KeywordToken(lexeme) if isStatementStart(lexeme) => {
          StatementList.parse(tokenizer, tree.appendTree(Declaration("statements")))
          parse(tokenizer, tree)
        }
        case SymbolToken('}') => {
          tree.appendLeaf(tokenizer.next)
          return
        }
      }
    }
  }

  private object VariableDeclaration extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken("var") => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case headToken: Token if isType(headToken) => {
          tree.appendLeaf(tokenizer.next) //append type
          tree.appendLeaf(tokenizer.next) //append varName
          parse(tokenizer, tree)
        }
        case SymbolToken(',') => {
          tree.appendLeaf(tokenizer.next) //append comma
          tree.appendLeaf(tokenizer.next) //append varName
          parse(tokenizer, tree)
        }
        case SymbolToken(';') => {
          tree.appendLeaf(tokenizer.next)
          return
        }
      }
    }
  }

  private object StatementList extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken("let") => {
          LetStatement.parse(tokenizer, tree.appendTree(Declaration("letStatement")))
          parse(tokenizer, tree)
        }
        case KeywordToken("if") => {
          IfStatement.parse(tokenizer, tree.appendTree(Declaration("ifStatement")))
          parse(tokenizer, tree)
        }
        case KeywordToken("while") => {
          WhileStatement.parse(tokenizer, tree.appendTree(Declaration("whileStatement")))
          parse(tokenizer, tree)
        }
        case KeywordToken("do") => {
          DoStatement.parse(tokenizer, tree.appendTree(Declaration("doStatement")))
          parse(tokenizer, tree)
        }
        case KeywordToken("return") => {
          ReturnStatement.parse(tokenizer, tree.appendTree(Declaration("returnStatement")))
          parse(tokenizer, tree)
        }
        case SymbolToken('}') => return //appended higher up
      }
    }
  }

  private object LetStatement extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken("let") => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case IdentifierToken(_) => {
          tree.appendLeaf(tokenizer.next) //append varName
          if(tokenizer.head == SymbolToken('[')) {
            tree.appendLeaf(tokenizer.next) //append [
            Expression.parse(tokenizer, tree.appendTree(Declaration("expression")))
            tree.appendLeaf(tokenizer.next) //append ]
          }
          parse(tokenizer, tree)
        }
        case SymbolToken('=') => {
          tree.appendLeaf(tokenizer.next) //append =
          Expression.parse(tokenizer, tree.appendTree(Declaration("expression")))
          tree.appendLeaf(tokenizer.next) //append ;
          return
        }
      }
    }
  }

  private object IfStatement extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken("if") => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case SymbolToken('(') => {
          tree.appendLeaf(tokenizer.next) //append (
          Expression.parse(tokenizer, tree.appendTree(Declaration("expression")))
          tree.appendLeaf(tokenizer.next) //append )
          parse(tokenizer, tree)
        }
        case SymbolToken('{') => {
          tree.appendLeaf(tokenizer.next) //append {
          StatementList.parse(tokenizer, tree.appendTree(Declaration("statements")))
          tree.appendLeaf(tokenizer.next) //append }
          if(tokenizer.head != KeywordToken("else")) return
          else parse(tokenizer, tree)
        }
        case KeywordToken("else") => {
          tree.appendLeaf(tokenizer.next) //append else
          tree.appendLeaf(tokenizer.next) //append {
          StatementList.parse(tokenizer, tree.appendTree(Declaration("statements")))
          tree.appendLeaf(tokenizer.next) //append }
          return
        }
      }
    }
  }

  private object WhileStatement extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken("while") => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case SymbolToken('(') => {
          tree.appendLeaf(tokenizer.next) //append (
          Expression.parse(tokenizer, tree.appendTree(Declaration("expression")))
          tree.appendLeaf(tokenizer.next) //append )
          parse(tokenizer, tree)
        }
        case SymbolToken('{') => {
          tree.appendLeaf(tokenizer.next) //append {
          StatementList.parse(tokenizer, tree.appendTree(Declaration("statements")))
          tree.appendLeaf(tokenizer.next) //append }
          return
        }
      }
    }
  }

  private object DoStatement extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken("do") => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case IdentifierToken(_) => {
          SubroutineCall.parse(tokenizer, tree)
          parse(tokenizer, tree)
        }
        case SymbolToken(';') => {
          tree.appendLeaf(tokenizer.next)
          return
        }
      }
    }
  }

  private object ReturnStatement extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case KeywordToken("return") => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case headToken: Token if isExpressionStart(headToken) => {
          Expression.parse(tokenizer, tree.appendTree(Declaration("expression")))
          parse(tokenizer, tree)
        }
        case SymbolToken(';') => {
          tree.appendLeaf(tokenizer.next)
          return
        }
      }
    }    
  }

  private object SubroutineCall extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case IdentifierToken(_) => {
          tree.appendLeaf(tokenizer.next)
          parse(tokenizer, tree)
        }
        case SymbolToken('.') => {
          tree.appendLeaf(tokenizer.next) //append .
          tree.appendLeaf(tokenizer.next) //append subroutineName
          parse(tokenizer, tree)
        }
        case SymbolToken('(') => {
          tree.appendLeaf(tokenizer.next) //append (
          ExpressionList.parse(tokenizer, tree.appendTree(Declaration("expressionList")))
          tree.appendLeaf(tokenizer.next) //append )
          return
        }
      }
    }
  }

  private object ExpressionList extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case headToken: Token if isExpressionStart(headToken) => {
          Expression.parse(tokenizer, tree.appendTree(Declaration("expression")))
          parse(tokenizer, tree)
        }
        case SymbolToken(',') => {
          tree.appendLeaf(tokenizer.next) //append ,
          Expression.parse(tokenizer, tree.appendTree(Declaration("expression")))
          parse(tokenizer, tree)
        }
        case SymbolToken(lexeme) if isExpressionEnd(lexeme) => return //appended higher up
      }
    }
  }

  private object Expression extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case SymbolToken(lexeme) if isExpressionOperator(lexeme) => {
          //binary - is considered before unary -
          tree.appendLeaf(tokenizer.next) //append op
          Term.parse(tokenizer, tree.appendTree(Declaration("term")))
          parse(tokenizer, tree)
        }
        case headToken: Token if isExpressionStart(headToken) => {
          Term.parse(tokenizer, tree.appendTree(Declaration("term")))
          parse(tokenizer, tree)
        }
        case SymbolToken(lexeme) if isExpressionEnd(lexeme) => return //appended higher up
      }
    }
  }

  private object Term extends Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) {
      tokenizer.head match {
        case IntegerToken(_) => tree.appendLeaf(tokenizer.next)
        case StringToken(_) => tree.appendLeaf(tokenizer.next)
        case KeywordToken(lexeme) if isKeywordConstant(lexeme) => tree.appendLeaf(tokenizer.next)
        case IdentifierToken(_) => {
          //handles variable and array dereference, subroutine calls; all start with an identifier
          tree.appendLeaf(tokenizer.next)
          if(tokenizer.head == SymbolToken('[')) {
            tree.appendLeaf(tokenizer.next) //append [
            Expression.parse(tokenizer, tree.appendTree(Declaration("expression")))
            tree.appendLeaf(tokenizer.next) //append ]
          } else if(tokenizer.head == SymbolToken('(')) {
            SubroutineCall.parse(tokenizer, tree)
          } else if(tokenizer.head == SymbolToken('.')) {
            tree.appendLeaf(tokenizer.next) //append .
            parse(tokenizer, tree) //will handle subroutineCall normally
          }
        }
        case SymbolToken('(') => {
          tree.appendLeaf(tokenizer.next) //append (
          Expression.parse(tokenizer, tree.appendTree(Declaration("expression")))
          tree.appendLeaf(tokenizer.next) //append )
        }
        case SymbolToken(lexeme) if isUnaryOperator(lexeme) => {
          tree.appendLeaf(tokenizer.next) //append the unary operator
          parse(tokenizer, tree.appendTree(Declaration("term")))
        }
      }
    }
  }

  private def isClassVarDecStart(lexeme: String) = "(static|field)".r.findFirstIn(lexeme) != None

  private def isSubroutineStart(lexeme: String) = {
    "(constructor|function|method)".r.findFirstIn(lexeme) != None
  }

  private def isSubroutineReturnType(token: Token) = token match {
    case KeywordToken("void") => true
    case headToken: Token if isType(headToken) => true
    case _ => false
  }

  private def isType(token: Token) = token match {
    case KeywordToken(lexeme) if "(int|char|boolean)".r.findFirstIn(lexeme) != None => true
    case IdentifierToken(_) => true
    case _ => false
  }

  private def isStatementStart(lexeme: String) = {
    "(let|if|while|do|return)".r.findFirstIn(lexeme) != None
  }

  private def isExpressionStart(token: Token) = token match {
    case IntegerToken(_) => true
    case StringToken(_) => true
    case KeywordToken(lexeme) if isKeywordConstant(lexeme) => true
    case IdentifierToken(_) => true
    case SymbolToken('(') => true
    case SymbolToken(lexeme) if isUnaryOperator(lexeme) => true
    case _ => false
  }

  private def isKeywordConstant(lexeme: String) = {
    "(true|false|null|this)".r.findFirstIn(lexeme) != None
  }

  private def isUnaryOperator(lexeme: Char) = "(-|~)".r.findFirstIn(lexeme.toString) != None

  private def isExpressionOperator(lexeme: Char) = {
    """(\+|-|\*|/|&|\||<|>|=)""".r.findFirstIn(lexeme.toString) != None
  }

  private def isExpressionEnd(lexeme: Char) = {
    """(\]|\)|;|,)""".r.findFirstIn(lexeme.toString) != None
  }
}