package tinybasic

import java.io.{Closeable, IOException, Reader}

class Tokenizer(val reader: Reader) extends Closeable {

  private def peek(): Option[Char] = {
    try {
      reader.mark(1)
      val next = reader.read()
      if (next == -1) None else Some(next.asInstanceOf[Char])
    } finally {
      reader.reset()
    }
  }

  private def string(): Token = {
    val value = Iterator.continually(reader.read())
                .takeWhile(_ != -1)
                .map(_.asInstanceOf[Char])
                .takeWhile(_ != '"')
                .mkString
    Literal(value)
  }

  def operator(ch: Char): Token = {
    peek() match {
      case None                      => throw new IOException("Unexpected Eof.")
      case Some(n) if n.isWhitespace => if (ch == '>') GreaterThan else LessThan
      case Some(n)                   => n match {
        case '='              => if (ch == '>') GreaterThanEqual else LessThanEqual
        case '>' if ch == '<' => NotEqual
        case '<' if ch == '>' => NotEqual
        case x                => throw new IOException(s"Unexpected character '$x' in operator.")
      }
    }
  }

  def number(d: Char): Token = {
    val value = Iterator.continually(reader.read())
                .takeWhile(_ != -1)
                .map(_.asInstanceOf[Char])
                .takeWhile(_.isDigit)
                .mkString(d.toString, "", "")
    Numeric(value.toInt) // no big numbers
  }

  def upper(c: Char): Token = {
    peek() match {
      case None                      => Variable(c.toString)
      case Some(n) if n.isWhitespace => Variable(c.toString)
      case Some(_)                   =>
        val kind = Iterator.continually(reader.read())
                   .takeWhile(_ != -1)
                   .map(_.asInstanceOf[Char])
                   .takeWhile(_.isUpper)
                   .mkString
        Keyword(s"$c$kind")
    }
  }

  def next(): Token = {
    var n: Int = -1
    do {
      n = reader.read()
      if (n == -1) return Eof
    } while (n.asInstanceOf[Char].isWhitespace)

    val ch = n.asInstanceOf[Char]
    ch match {
      case '\n'           => NewLine
      case '+'            => Add
      case '-'            => Sub
      case '*'            => Mul
      case '/'            => Div
      case '='            => Equal
      case ','            => Comma
      case '('            => LParen
      case ')'            => RParen
      case '"'            => string()
      case '>' | '<'      => operator(ch)
      case d if d.isDigit => number(d)
      case c if c.isUpper => upper(c)
      case x              => throw new UnsupportedOperationException(s"Unexpected character: '$x'")
    }
  }

  override def close(): Unit = reader.close()
}
