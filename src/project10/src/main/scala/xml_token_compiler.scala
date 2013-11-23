package compiler

import scala.collection.mutable.LinkedList

/** A `CompilerComponent` that produces an XML list of tokens.
  */
class XMLTokenCompiler
    extends CompilerComponent
    with ParserComponent
    with GeneratorComponent {

  //use a parser that only percieves tokens, and not any lexical structure
  val parser = new Parser {
    def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue]) = {
      tokenizer.foreach(tree.appendLeaf(_))
    }
  }

  //use a generator that produces an XML string
  val generator = new XmlGenerator
}