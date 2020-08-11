package value

import expression._
import context.Environment

class Thunk(val params: List[Identifier], val bod: Expression, val env: Environment) extends Closure(params, bod, env) {
  var cache: Value = null
  
  def apply(): Value = {
    if (cache == null) {
      cache = super.apply(Nil, env)
    }
    cache
  }
}