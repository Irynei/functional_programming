package main.tiny_language

final class Machine(var env: Map[String, Expr]) {

  def run(expr: Expr): Option[Expr] = {
    println(expr)

    if (expr.isReducible)
      try
        run(reductionStep(expr))
      catch {
        case exception: CustomException => println(exception.getMessage)
          None
      }
    else Option(expr)
  }

  def reductionStep(expr: Expr): Expr = {
    def reduceBinary(op: (Expr, Expr) => Expr, l: Expr, r: Expr): Expr = {
      if (l.isReducible) op(reductionStep(l), r)
      else if (r.isReducible) op(l, reductionStep(r))
      else expr.eval
    }
    expr match {
      case Var(x) =>
        if (env.contains(x)) env(x)
        else throw CustomException(s"Error: name `$x` is not defined")
      case Sum(l, r) => reduceBinary(Sum.apply, l, r)
      case Prod(l, r) => reduceBinary(Prod.apply, l, r)
      case Less(l, r) => reduceBinary(Less.apply, l, r)
      case IfElse(c, f, s) =>
        if (c.isReducible) IfElse(reductionStep(c), f, s)
        else if (c.castToBool) f else s
    }
  }

  def run(statement: Statement): Option[Statement] = {
    println(s"Env: ${this.env}")
    println(s"Exec statement: $statement")

    if (statement.isReducible)
      try
        run(reductionStep(statement))
      catch {
        case exception: CustomException => println(exception.getMessage)
          None
      }
    else Option(statement)

  }

  def reductionStep(statement: Statement): Statement = {
      statement match {
        case Assign(n, v) => {
          if(v.isReducible) Assign(n, reductionStep(v))
          else {
            env += n -> v
            DoNothing()
          }
        }
        case IfElseStatement(c, f, s) => {
          if (c.isReducible) IfElseStatement(reductionStep(c), f, s)
          else if (c.castToBool) f else s
        }
        case While(c, s) => {
          val condition = run(c).getOrElse(Bool(false))
          if (condition.castToBool) {
            run(s)
            While(c, s)
          } else DoNothing()
        }
        case Sequence(seqList) => {
          if (seqList.isEmpty) DoNothing()
          else {
            run(seqList.head)
            reductionStep(Sequence(seqList.tail))
          }
        }

      }
    }
}
