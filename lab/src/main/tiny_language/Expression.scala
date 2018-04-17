package main.tiny_language

sealed trait Expr {

  def eval: Expr = this match {
    case Number(_) | Bool(_) => this
    case Sum(l, r) => Number(l.eval + r.eval)
    case Prod(l, r) => Number(l.eval * r.eval)
    case Less(l, r) => Bool(l.eval < r.eval)
//    case IfElse(c, f, s) => if (c.castToBool) f.eval else s.eval
  }

  def castToBool: Boolean = this match {
      case Bool(n) => n
      case _ => throw CustomException(s"Error: Cannot convert `$this` to `Boolean` type")
    }

  def castToInt: Int = this match {
      case Number(n) => n
      case _ => throw CustomException(s"Error: Cannot convert `$this` to `Int` type")
    }

  def +(that: Expr): Int = {
    this.castToInt + that.castToInt
  }

  def *(that: Expr): Int = {
    this.castToInt * that.castToInt
  }

  def <(that: Expr): Boolean = {
    this.castToInt < that.castToInt
  }

  def isReducible: Boolean = this match {
      case Number(_) | Bool(_) => false
      case _ => true
    }

  def show: String = {
    def addParenthesis(expr: Expr): String = "(" + expr.show + ")"
    this match {
      case Bool(b) => b.toString
      case Number(n) => n.toString
      case Var(x) => x
      case Sum(l, r) => l.show + " + " + r.show
      case Prod(l, r) => (l, r) match {
        case (Sum(_, _), Sum(_, _)) => addParenthesis(l) + " * " + addParenthesis(r)
        case (Sum(_, _), _) => addParenthesis(l) + " * " + r.show
        case (_, Sum(_, _)) => l.show + " * " + addParenthesis(r)
        case _ => l.show + " * " + r.show
      }
      case Less(l, r) => l.show + " < " + r.show
      case IfElse(c, f, s) =>  "if " + addParenthesis(c) + " then " + f.show + " else " + s.show
      case _ => "Unknown"
    }
  }

  override def toString: String = this.show
}

case class Number(n: Int) extends Expr

case class Var(x: String) extends Expr

case class Sum(l: Expr, r: Expr) extends Expr

case class Prod(l: Expr, r: Expr) extends Expr

case class Bool(b: Boolean) extends Expr

case class Less(l: Expr, r: Expr) extends Expr

case class IfElse(condition: Expr, first: Expr, second: Expr) extends Expr