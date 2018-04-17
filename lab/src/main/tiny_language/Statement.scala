package main.tiny_language

sealed trait Statement {
  def addParenthesis(item: Either[Expr, Statement]): String = item match {
    case Left(x) => "(" + x.show + ")"
    case Right(x) => "(" + x.show + ")"
  }
  def show: String = {
    this match {
      case DoNothing() => "nothing \n"
      case Assign(n, v) => s"assign ${n} = ${v}"
      case IfElseStatement(c, f, s) =>
        "if " + addParenthesis(Left(c)) + " then " + addParenthesis(Right(f)) + " else " + addParenthesis(Right(s))
      case While(c, s) =>
        "while " + addParenthesis(Left(c)) + " do " + addParenthesis(Right(s))
      case Sequence(seqList) =>
        if (seqList.isEmpty) "END OF SEQUENCE\n"
        else "BEGIN OF SEQUENCE"
    }
  }

  def isReducible: Boolean = this match {
    case DoNothing() => false
    case _ => true
  }
  override def toString: String = this.show

}

case class DoNothing() extends Statement
case class Assign(name: String, value: Expr) extends Statement
case class IfElseStatement(cond: Expr, first: Statement, second: Statement) extends Statement
case class While(cond: Expr, statement: Statement) extends Statement
case class Sequence(statements: List[Statement]) extends Statement