package compiler

/** A `CompilerComponent` that produces an XML tree according to the Jack grammar.
  */
class XMLTreeCompiler
    extends CompilerComponent
    with ParserComponent
    with GeneratorComponent {

  //use a parser that percieves the language structure
  val parser = JackParser

  //use a generator that produces an XML string
  val generator = new XmlGenerator
}