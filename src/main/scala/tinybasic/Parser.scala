package tinybasic

import scala.collection.mutable.ListBuffer

class Parser(val tokens: Iterator[Token]) {

  private var token: Token = tokens.next()

  private def mismatch(expected: String, actual: Any): Throwable = {
    new UnsupportedOperationException(s"Expected '$expected' got '$actual'")
  }

  def parse(): Program = {
    val lines = Iterator.continually(line()).takeWhile(_.isDefined).map(_.get).toList
    new Program(lines)
  }

  private def consume(): Token = {
    token match {
      case Eof => Eof
      case cur =>
        token = tokens.next()
        cur
    }
  }

  private def line(): Option[Line] = {
    consume() match {
      case Eof          => None // we're done
      case Numeric(num) => Some(Line(num, statement()))
      case x            => throw mismatch("line number", x)
    }
  }

  private def statement(): Statement = {
    val keyword = consume() match {
      case Keyword(kind) => kind
      case x             => throw mismatch("keyword", x)
    }

    keyword match {
      case "PRINT"                  => Print(exprs())
      case "IF"                     => conditional()
      case "GOTO"                   => Goto(expr())
      case "GOSUB"                  => Gosub(expr())
      case "INPUT"                  => Input(vars())
      case "LET"                    => assign()
      case "RETURN"                 => Return
      case "END"                    => End
      case "CLEAR" | "LIST" | "RUN" => throw new UnsupportedOperationException(keyword)
      case x                        => throw mismatch("keyword", x)
    }
  }

  private def conditional(): Statement = {
    val lhs = expr()
    val op = consume() match {
      case Equal            => Eq
      case NotEqual         => Ne
      case GreaterThan      => Gt
      case GreaterThanEqual => Ge
      case LessThan         => Lt
      case LessThanEqual    => Le
      case x                => throw mismatch("relative operator", x)
    }
    val rhs = expr()

    consume() match {
      case Keyword("THEN") => // this is what we want
      case x               => throw mismatch("THEN", x)
    }

    If(op, lhs, rhs, statement())
  }

  private def assign(): Statement = {
    val target = consume() match {
      case Variable(name) => name
      case x              => throw mismatch("variable", x)
    }

    consume() match {
      case Equal =>
      case x     => throw mismatch("=", x)
    }

    Assign(target, expr())
  }

  private def vars(): List[String] = {
    val vars = ListBuffer[String]()

    do {
      if (vars.nonEmpty) {
        token match {
          case Comma => consume()
          case _     => return vars.toList
        }
      }

      consume() match {
        case Variable(name) => vars.append(name)
        case x              => throw mismatch("variable", x)
      }
    } while (true)

    vars.toList
  }

  private def exprs(): List[Expr] = {
    val exprs = ListBuffer[Expr]()

    do {
      if (exprs.nonEmpty) {
        token match {
          case Comma => consume()
          case _     => return exprs.toList
        }
      }

      token match {
        case Literal(value) =>
          consume()
          exprs.append(StringConst(value))
        case _              =>
          exprs.append(expr())
      }
    } while (true)

    exprs.toList
  }

  private def expr(): Expr = {
    var expr = token match {
      case Add =>
        consume()
        term()
      case Sub =>
        consume()
        Negate(term())
      case _   => term()
    }

    while (token == Add || token == Sub) {
      val op = consume() match {
        case Add => AddOp
        case Sub => SubOp
        case _   => throw new RuntimeException("???")
      }
      expr = BinaryOp(op, expr, term())
    }

    expr
  }

  private def term(): Expr = {
    var expr = factor()

    while (token == Mul || token == Div) {
      val op = consume() match {
        case Mul => MulOp
        case Div => DivOp
        case _   => throw new RuntimeException("???")
      }
      expr = BinaryOp(op, expr, factor())
    }

    expr
  }

  private def factor(): Expr = {
    consume() match {
      case Variable(name) => Var(name)
      case Numeric(value) => IntConst(value)
      case LParen         =>
        try {
          expr()
        } finally {
          consume() match {
            case RParen => // this is what we want
            case x      => throw mismatch("rparen", x)
          }
        }
      case x              => throw mismatch("var | number | lparen", x)
    }
  }
}
