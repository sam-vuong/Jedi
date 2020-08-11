package expression

import value.Value
import context.Environment
import value.Text

case class MakeText(val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    val res = new Text(body)
    res
  }
}