package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    namedExpressions.map { case (k, v) => (k, Signal(eval(v(), namedExpressions))) }
  }

  def checkCycle(name: String, references: Map[String, Signal[Expr]] ):Boolean = {
    def checkExpr(expr: Expr):Boolean = expr match {
      case r:Ref => if (name == r.name) true else checkExpr(getReferenceExpr(r.name, references))
      case e:Plus => checkExpr(e.a) || checkExpr(e.b)
      case e:Minus  => checkExpr(e.a) || checkExpr(e.b)
      case e:Times => checkExpr(e.a) || checkExpr(e.b)
      case e:Divide => checkExpr(e.a) || checkExpr(e.b)
      case _ => false
    }
    checkExpr( getReferenceExpr(name, references) )
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case l: Literal => l.v
    case r: Ref =>
      if (checkCycle(r.name, references)) Double.NaN
      else eval( getReferenceExpr(r.name, references), references)
    case p: Plus => eval(p.a, references) + eval(p.b, references)
    case m: Minus => eval(m.a, references) - eval(m.b, references)
    case t: Times => eval(t.a, references) * eval(t.b, references)
    case d: Divide => eval(d.b, references) match {
      case 0 => Double.NaN
      case x => eval(d.a, references) / x
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
