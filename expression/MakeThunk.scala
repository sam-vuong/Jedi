package expression

import value.Value
import context.Environment
import value.Thunk

case class MakeThunk(val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    val res = new Thunk(Nil, body, env)
    res
  }
}