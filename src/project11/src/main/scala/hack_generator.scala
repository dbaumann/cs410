package compiler

import scala.collection.mutable._

/** A `Generator` which produces indented XML containing symbol table metadata.
  */
object HackGenerator extends Generator {
  import scala.xml._
  import SymbolTable.VarKind
  import VMWriter.SegmentKind
  
  def generateFromTree(parseTree: TreeNode[TreeValue]) = { 
    new PrettyPrinter(80, 2).format(generateXmlFor(parseTree))
    VMWriter.result
  }

  private def generateXmlFor(parseTree: TreeNode[TreeValue]): Elem = {
    parseTree match {
      case TreeNode(Declaration(name), children) => {
        var newNode = name match {
          case "varDec" => genVarDec(children.get.iterator.buffered)
          case "parameterList" => genParameterList(children.get.iterator.buffered)
          case "classVarDec" => genClassVarDec(children.get.iterator.buffered)
          case "class" => genClassDec(children.get.iterator.buffered)
          case "subroutineDec" => genSubroutineDec(children.get.iterator.buffered)
          case "doStatement" => genDoStatement(children.get.iterator.buffered)
          case "term" => genTerm(children.get.iterator.buffered)

          case "expression" => {
            writeExpression(children.get.iterator.buffered)
            <node></node>
          }
          case "returnStatement" => {
            VMWriter.writeReturn
            <node></node>
          }

          case otherName @ _ => {

            otherName match {
              case "subroutineBody" => {
                VMWriter.writeFunction(SymbolTable.scopeName, SymbolTable.varCount(VarKind.ARG))
              }
              case _ => //all other declarations
            }

            var template = <node></node>
            children.get.foreach { node: TreeNode[TreeValue] =>
              template = addChild(template, generateXmlFor(node))
            }
            template
          }
        }
        
        newNode.copy(label=name)
      }
      case TreeNode(KeywordToken(lexeme), _) => {
        <keyword> {lexeme} </keyword>
      }
      case TreeNode(SymbolToken(lexeme), _) => {
        <symbol> {lexeme} </symbol>
      }
      case TreeNode(IntegerToken(lexeme), _) => {
        <integerConstant> {lexeme} </integerConstant>
      }
      case TreeNode(StringToken(lexeme), _) => {
        <stringConstant> {lexeme} </stringConstant>
      }
      case TreeNode(IdentifierToken(lexeme), _) => {
        generateIdentifierXml(lexeme, "var", "false", SymbolTable.indexOf(lexeme).toString)
      }
    }
  }

  private def genVarDec(componentIterator: BufferedIterator[TreeNode[TreeValue]]) = {
    var result = <node></node>

    result = addChild(result, generateXmlFor(componentIterator.next)) //var keyword

    //could be a user-defined type
    val varType = componentIterator.head match {
      case TreeNode(KeywordToken(varType), _) => {
        result = addChild(result, generateXmlFor(componentIterator.next))
        varType
      }
      case TreeNode(IdentifierToken(varType), _) => {
        result = addChild(result, generateIdentifierXml(varType, "class", "false"))
        componentIterator.next
        varType
      }
    }

    //could have more than one component
    while(componentIterator.hasNext) {
      val TreeNode(IdentifierToken(varName), _) = componentIterator.next
      result = addChild(result, generateIdentifierXml(
                                    varName,
                                    "var",
                                    "true",
                                    SymbolTable.varCount(VarKind.VAR).toString))
      SymbolTable.define(varName, varType, "var")

      result = addChild(result, generateXmlFor(componentIterator.next)) //, or ; symbol
    }

    result
  }

  private def genParameterList(componentIterator: BufferedIterator[TreeNode[TreeValue]]) = {
    var result = <node></node>

    while(componentIterator.hasNext) {

      //could be a user-defined type
      val varType = componentIterator.head match {
        case TreeNode(KeywordToken(varType), _) => {
          result = addChild(result, generateXmlFor(componentIterator.next))
          varType
        }
        case TreeNode(IdentifierToken(varType), _) => {
          result = addChild(result, generateIdentifierXml(varType, "class", "false"))
          componentIterator.next
          varType
        }
      }

      val TreeNode(IdentifierToken(varName), _) = componentIterator.next
      result = addChild(result, generateIdentifierXml(
                                    varName,
                                    "arg",
                                    "true",
                                    SymbolTable.varCount(VarKind.ARG).toString))
      SymbolTable.define(varName, varType, "arg")

      if(componentIterator.hasNext) {
        result = addChild(result, generateXmlFor(componentIterator.next)) //, symbol
      }
    }

    result
  }

  private def genClassVarDec(componentIterator: BufferedIterator[TreeNode[TreeValue]]) = {
    var result = <node></node>

    val TreeNode(KeywordToken(keyword), _) = componentIterator.head //field or static keyword
    result = addChild(result, generateXmlFor(componentIterator.next))

    //could be a user-defined type
    val varType = componentIterator.head match {
      case TreeNode(KeywordToken(varType), _) => {
        result = addChild(result, generateXmlFor(componentIterator.next))
        varType
      }
      case TreeNode(IdentifierToken(varType), _) => {
        result = addChild(result, generateIdentifierXml(varType, "class", "false"))
        componentIterator.next
        varType
      }
    }

    //could have more than one component
    while(componentIterator.hasNext) {
      val TreeNode(IdentifierToken(varName), _) = componentIterator.next
      result = addChild(result, generateIdentifierXml(
                                    varName,
                                    keyword,
                                    "true",
                                    SymbolTable.varCount(VarKind.withName(keyword)).toString))
      SymbolTable.define(varName, varType, keyword)

      result = addChild(result, generateXmlFor(componentIterator.next)) //, or ; symbol
    }

    result
  }

  private def genClassDec(componentIterator: BufferedIterator[TreeNode[TreeValue]]) = {
    var result = <node></node>

    result = addChild(result, generateXmlFor(componentIterator.next)) //class keyword

    val TreeNode(IdentifierToken(varName), _) = componentIterator.next
    result = addChild(result, generateIdentifierXml(varName, "class", "true"))

    //reset the symbol table for this scope
    SymbolTable.startClass(varName)

    while(componentIterator.hasNext) {
      result = addChild(result, generateXmlFor(componentIterator.next))
    }
    
    result
  }

  private def genSubroutineDec(componentIterator: BufferedIterator[TreeNode[TreeValue]]) = {
    var result = <node></node>

    //constructor, method, function keyword
    result = addChild(result, generateXmlFor(componentIterator.next))

    //could be a user-defined type
    componentIterator.head match {
      case TreeNode(KeywordToken(retType), _) => {
        result = addChild(result, generateXmlFor(componentIterator.next))
      }
      case TreeNode(IdentifierToken(retType), _) => {
        result = addChild(result, generateIdentifierXml(retType, "class", "false"))
        componentIterator.next
      }
    }

    val TreeNode(IdentifierToken(funName), _) = componentIterator.next
    result = addChild(result, generateIdentifierXml(funName, "subroutine", "true"))

    //reset the symbol table for this scope
    SymbolTable.startSubroutine(funName)

    while(componentIterator.hasNext) {
      result = addChild(result, generateXmlFor(componentIterator.next))
    }
    
    result
  }

  private def genDoStatement(componentIterator: BufferedIterator[TreeNode[TreeValue]]) = {
    var result = <node></node>

    result = addChild(result, generateXmlFor(componentIterator.next)) //do keyword

    var methodName = ""

    val TreeNode(IdentifierToken(firstIdentifier), _) = componentIterator.next

    val TreeNode(nextToken, _) = componentIterator.head
    if(nextToken == SymbolToken('.')) {
      //method called with another object
      result = addChild(result, generateIdentifierXml(firstIdentifier, "class", "false"))
      result = addChild(result, generateXmlFor(componentIterator.next)) //. symbol

      val TreeNode(IdentifierToken(funName), _) = componentIterator.next
      result = addChild(result, generateIdentifierXml(funName, "subroutine", "false"))

      methodName = firstIdentifier + "." + funName
    } else {
      //method called with this object
      result = addChild(result, generateIdentifierXml(firstIdentifier, "subroutine", "false"))

      methodName = firstIdentifier
    }

    var expressionCount = 0

    while(componentIterator.hasNext) {
      val TreeNode(treeValue, children) = componentIterator.head
      if(treeValue == Declaration("expressionList")) {
        expressionCount = children.get.length
      }
      result = addChild(result, generateXmlFor(componentIterator.next))
    }

    VMWriter.writeCall(methodName, expressionCount)
    VMWriter.writePop(SegmentKind.TEMP, 0)

    result
  }

  private def genTerm(componentIterator: BufferedIterator[TreeNode[TreeValue]]) = {
    var result = <node></node>

    componentIterator.head match {
      case TreeNode(IdentifierToken(firstIdentifier), _) => {
        componentIterator.next
        if(!componentIterator.hasNext) {
          //variable being recalled
          result = addChild(result, generateIdentifierXml(
                                        firstIdentifier,
                                        "var",
                                        "false",
                                        SymbolTable.indexOf(firstIdentifier).toString))
        } else {
          val TreeNode(nextToken, _) = componentIterator.head
          if(nextToken == SymbolToken('.')) {
            //method called with another object
            result = addChild(result, generateIdentifierXml(firstIdentifier, "class", "false"))
            result = addChild(result, generateXmlFor(componentIterator.next)) //. symbol

            val TreeNode(IdentifierToken(funName), _) = componentIterator.next
            result = addChild(result, generateIdentifierXml(funName, "subroutine", "false"))
          } else {
            //method called with this object
            result = addChild(result, generateIdentifierXml(firstIdentifier, "subroutine", "false"))
          }
        }        
      }
      case _ => {
        while(componentIterator.hasNext) {
          result = addChild(result, generateXmlFor(componentIterator.next))
        }
      }
    }

    result
  }

  private def writeExpression(componentIterator: BufferedIterator[TreeNode[TreeValue]]) {
    componentIterator.next match {
      //term or (term op term)
      case TreeNode(Declaration("term"), firstOperandContents) => {
        //process firstOperandContents
        writeTerm(firstOperandContents.get.iterator.buffered)

        //op (binary)
        if(componentIterator.hasNext) componentIterator.next match {
          case TreeNode(SymbolToken(operator), _) => {

            val TreeNode(Declaration("term"), secondOperandContents) = componentIterator.next
            //process secondOperandContents
            writeTerm(secondOperandContents.get.iterator.buffered)

            //write call to operator
            VMWriter.writeArithmetic(operator)
          }
        }
      }

      //op (unary)
      case TreeNode(SymbolToken(operator), _) => {
        val TreeNode(Declaration("term"), firstOperandContents) = componentIterator.next
        //process firstOperandContents
        writeTerm(firstOperandContents.get.iterator.buffered)

        //write call to operator
        VMWriter.writeArithmetic(operator)
      }
    }
  }

  private def writeTerm(componentIterator: BufferedIterator[TreeNode[TreeValue]]) {
    componentIterator.next match {
      case TreeNode(SymbolToken('('), _) => {
        val TreeNode(Declaration(name), expressionContents) = componentIterator.next
        writeExpression(expressionContents.get.iterator.buffered)
        componentIterator.next //) symbol
      }
      case TreeNode(IdentifierToken(firstIdentifier), _) => {
        if(!componentIterator.hasNext) {
          //variable being recalled
          VMWriter.writePush(
              VMWriter.SegmentKind.fromSymbolKind(SymbolTable.kindOf(firstIdentifier).get),
              SymbolTable.indexOf(firstIdentifier))
        }
      }
      case TreeNode(IntegerToken(constant), _) => {
        VMWriter.writePush(SegmentKind.CONST, constant)
      }

    }
  }

  private def addChild(parent: Elem, newEntry: Elem): Elem = parent match {
    case <node>{ ch @ _* }</node> => <node>{ ch }{ newEntry }</node>
  }

  private def generateIdentifierXml(
      name: String,
      category: String,
      declared: String,
      index: String = "") = {

    <identifier> {name} </identifier> %
      Attribute(None, "cat", Text(category), Null) %
      Attribute(None, "declared", Text(declared), Null) %
      Attribute(None, "index", Text(index), Null)
  }
}