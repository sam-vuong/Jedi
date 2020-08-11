package expression

import context.Environment
import value._

case class Conditional(val condition: Expression, val consequent: Expression, val alternative: Expression = null) extends SpecialForm {
  //def execute(env: Environment): Value = null
  
  def execute(env: Environment): Value = {
    if (condition.execute(env).toString() == "true") {
      // execute consequent
      consequent.execute(env)
    }
    else {
      if (alternative == null) Notification.UNSPECIFIED
      else alternative.execute(env)
    }
  }
  
  override def toString = {
    var result = "if (" + condition + ") " + consequent
    if (alternative != null) result = result + " else " + alternative
    result
  }
}