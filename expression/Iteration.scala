package expression

import context.Environment
import value.Value
import context.TypeException
import value.Boole

case class Iteration(val condition: Expression, val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    var result: Value = null
    try {
      while (condition.execute(env).asInstanceOf[Boole].value) result = body.execute(env)
      result
    } 
    catch {
      case e: ClassCastException => throw new TypeException("Condition is not a Boole")
    }
  }
}