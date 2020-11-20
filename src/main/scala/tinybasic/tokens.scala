package tinybasic

sealed trait Token

case object NewLine extends Token
case object Eof extends Token

case object Add extends Token
case object Sub extends Token
case object Mul extends Token
case object Div extends Token

case object Equal extends Token
case object NotEqual extends Token

case object Comma extends Token
case object LParen extends Token
case object RParen extends Token

case class Literal(value: String) extends Token
case class Numeric(value: Int) extends Token

case object LessThan extends Token
case object GreaterThan extends Token
case object LessThanEqual extends Token
case object GreaterThanEqual extends Token

case class Variable(name: String) extends Token
case class Keyword(kind: String) extends Token