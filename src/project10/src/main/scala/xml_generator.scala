package compiler

/** A `Generator` which produces indented XML.
  */
class XmlGenerator extends Generator {
  import scala.xml._
  
  def generateFromTree(parseTree: TreeNode[TreeValue]) = { 
    new PrettyPrinter(80, 2).format(generateXmlFor(parseTree))
  }

  private def generateXmlFor(parseTree: TreeNode[TreeValue]): Elem = {
    parseTree match {
      case TreeNode(Declaration(name), children) => {
        var template = <node></node>
        children.get.foreach { node: TreeNode[TreeValue] =>
          template = addChild(template, generateXmlFor(node))
        }
        template.copy(label=name)
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
        <identifier> {lexeme} </identifier>
      }
    }
  }

  private def addChild(parent: Elem, newEntry: Elem): Elem = parent match {
    case <node>{ ch @ _* }</node> => <node>{ ch }{ newEntry }</node>
  }
}