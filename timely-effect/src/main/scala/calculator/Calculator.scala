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
    namedExpressions.map({
      case (varName, signalExpr) =>
        varName -> Signal(eval(signalExpr(), namedExpressions))
    })
  }
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = expr match {
    case Literal(v) => v
    case Ref(v) => eval(getReferenceExpr(v,references),references.filterKeys(_ != v))
    case Plus(a,b) => eval(a,references) + eval(b,references)
    case Minus(a,b) => eval(a, references) -  eval(b, references)
    case Times(a,b) => eval(a, references) *  eval(b, references)
    case Divide(a,b) => eval(a, references) /  eval(b, references)
    case _ => Double.NaN
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */


  //abstract def fold[A1 >: A](z: A1)(op: (A1, A1) ⇒ A1): A1
/*
 * Folds the elements of this collection or iterator using the specified associative binary operator.
 *
 *A1: a type parameter for the binary operator, a supertype of A.
 *z: a neutral element for the fold operation; may be added to the result an arbitrary number of times, and
 *   must not change the result (e.g., Nil for list concatenation, 0 for addition, or 1 for multiplication).
 *op: a binary operator that must be associative.
 *returns: the result of applying the fold operator op between all the elements and z, or z if this collection or iterator is empty.
*/
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN) //z:  a neutral element for the fold operation;
                         // may be added to the result an arbitrary number of times, and must not change the result:
                                // (e.g., Nil for list concatenation, 0 for addition, or 1 for multiplication)
                         // returns if value(as opposed to key in Map) is empty
    } { exprSignal =>
      exprSignal()
    }
  }
}
