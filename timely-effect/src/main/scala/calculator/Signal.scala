package calculator

import scala.util.DynamicVariable

class Signal[T](expr: => T) {
  import Signal._
  private var myExpr: () => T = _
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  private var observed: List[Signal[_]] = Nil
  update(expr)

  protected def computeValue(): Unit = {
    for (sig <- observed)
      sig.observers -= this
    observed = Nil
    val newValue = caller.withValue(this)(myExpr())
    /* Disable the following "optimization" for the assignment, because we
     * want to be able to track the actual dependency graph in the tests.
     */
    //if (myValue != newValue) {
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    //}
  }

  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }

  def apply() = {
    observers += caller.value  //def value: T ... Retrieve the current value
    assert(!caller.value.observers.contains(this), "cyclic signal definition")
    caller.value.observed ::= this // a ::= b 即 a = a.::(b)
    myValue
  }
}

class Var[T](expr: => T) extends Signal[T](expr) {
  override def update(expr: => T): Unit = super.update(expr)
}

object Var {
  def apply[T](expr: => T) = new Var(expr)
}

object NoSignal extends Signal[Nothing](???) {
  override def computeValue() = ()
}

/*
 *DynamicVariables provide a binding mechanism where the current value is found through dynamic scope,
 *but where access to the variable itself is resolved through static scope.
 *The current value can be retrieved with the value method. New values should be pushed using
 * the withValue method. Values pushed via withValue only stay valid while the withValue's second argument,
 * a parameterless closure, executes. When the second argument finishes, the variable reverts to the previous value.
*/
object Signal {
  val caller = new DynamicVariable[Signal[_]](NoSignal)
  def apply[T](expr: => T) = new Signal(expr)
}
