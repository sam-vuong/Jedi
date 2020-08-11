package expression

import context.Environment
import value.Value
import value.Closure

case class Lambda(val parameters: List[Identifier], val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    Closure(parameters, body, env)
  }
}