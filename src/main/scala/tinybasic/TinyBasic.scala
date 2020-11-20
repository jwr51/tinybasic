package tinybasic

import java.nio.file.{Files, Paths}

object TinyBasic {

  def main(args: Array[String]): Unit = {
    val input = args.headOption match {
      case Some(arg) => Paths.get(arg)
      case None      =>
        println("Use as: tinybasic <input>")
        return
    }

    val name = input.getFileName.toString.takeWhile(_ != '.')

    // there must be a better way
    val tokenizer = new Tokenizer(Files.newBufferedReader(input))
    val tokens = LazyList.continually(tokenizer.next()).takeWhile(_ != Eof).appended(Eof)

    val parser = new Parser(tokens.iterator)
    val program = parser.parse()

    val code = CodeGenerator(name, program)
    Files.write(Paths.get(s"$name.class"), code)
  }
}