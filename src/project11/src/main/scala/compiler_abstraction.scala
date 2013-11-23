package compiler

import scala.collection.mutable.LinkedList

/** This type produces a syntax tree.
  */
abstract class Parser {
  /** Grows a syntax tree from a token iterator. The passed-in tree is expanded.
    * @param tokenizer a token iterator
    * @param a syntax tree
    * @return void
    */
  def parse(tokenizer: BufferedIterator[Token], tree: TreeNode[TreeValue])
}

/** This type produces string of compiled code.
  */
abstract class Generator {
  /** Produces a string of compiled code from a syntax tree.
    * @param parseTree a syntax tree
    * @return a string of compiled code
    */
  def generateFromTree(parseTree: TreeNode[TreeValue]): String
}


trait ParserComponent {
  val parser: Parser
}

trait GeneratorComponent {
  val generator: Generator
}

/** Abstract definition of the compilation process.
  */
trait CompilerComponent {
  self: ParserComponent with
        GeneratorComponent =>

  /** Compiles from a token iterator.
    * @param tokenizer a token iterator
    * @return a string of compiled code
    */
  def compile(tokenizer: Iterator[Token]): String = {
    val syntaxTree = TreeNode[TreeValue](Declaration("class"),
                                         Some(new LinkedList[TreeNode[TreeValue]]()))
    parser.parse(tokenizer.buffered, syntaxTree)
    generator.generateFromTree(syntaxTree)
  }
}