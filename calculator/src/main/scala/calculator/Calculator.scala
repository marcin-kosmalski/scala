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
    namedExpressions.map(keyValue => (keyValue._1, Var(eval(keyValue._2(), namedExpressions))))
  }

  def calcVal(expr: Expr, fromRef: Set[String], namedExpressions: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Ref(name) => {
        if (fromRef.contains(name)|| !namedExpressions.contains(name)) {
          Double.NaN
        } else {
          calcVal(namedExpressions(name)(), fromRef + name, namedExpressions)
        }
      }
      case Literal(v)   => v
      case Plus(a, b)   => calcVal(a, fromRef, namedExpressions) + calcVal(b, fromRef, namedExpressions)
      case Minus(a, b)  => calcVal(a, fromRef, namedExpressions) - calcVal(b, fromRef, namedExpressions)
      case Times(a, b)  => calcVal(a, fromRef, namedExpressions) * calcVal(b, fromRef, namedExpressions)
      case Divide(a, b) => calcVal(a, fromRef, namedExpressions) / calcVal(b, fromRef, namedExpressions)
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    calcVal(expr, Set(), references)
  }

  /**
   * Get the Expr for a referenced variables.
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
