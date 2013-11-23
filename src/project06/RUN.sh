if [ ! `(ls -A assembler)` ]; then
  scalac instructions.scala
  scalac parser.scala
  scalac application.scala
fi
scala assembler.Application $1