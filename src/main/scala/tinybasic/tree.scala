package tinybasic

sealed trait ArithmeticOperator
case object AddOp extends ArithmeticOperator
case object SubOp extends ArithmeticOperator
case object MulOp extends ArithmeticOperator
case object DivOp extends ArithmeticOperator

sealed trait RelativeOperator
case object Eq extends RelativeOperator
case object Ne extends RelativeOperator
case object Gt extends RelativeOperator
case object Ge extends RelativeOperator
case object Lt extends RelativeOperator
case object Le extends RelativeOperator

sealed trait Expr

sealed trait Const extends Expr
case class StringConst(value: String) extends Expr
case class IntConst(value: Int) extends Expr

case class Negate(expr: Expr) extends Expr
case class BinaryOp(op: ArithmeticOperator, left: Expr, right: Expr) extends Expr

case class Var(name: String) extends Expr

sealed trait Statement
case class Print(exprs: List[Expr]) extends Statement
case class If(op: RelativeOperator, lhs: Expr, rhs: Expr, body: Statement) extends Statement

case class Goto(target: Expr) extends Statement
case class Gosub(target: Expr) extends Statement

case object Return extends Statement
case object End extends Statement

case class Input(vars: List[String]) extends Statement
case class Assign(name: String, value: Expr) extends Statement

case class Line(num: Int, statement: Statement)

class Program(val lines: List[Line])