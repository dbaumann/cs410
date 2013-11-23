package compiler

/** A `CompilerComponent` that produces Hack VM instructions.
  */
class HackCompiler
    extends CompilerComponent
    with ParserComponent
    with GeneratorComponent {

  //use a parser that percieves the language structure
  val parser = JackParser

  //use a generator that produces an XML string
  val generator = HackGenerator
}