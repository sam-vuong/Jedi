package expression

import context.Environment
import value.Value

// Make sure all expressions are only executed once!
// Might need to create a new environment in order to keep scope
case class Block(val exps: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    var tempEnv = new Environment(env)
    for (i <- 0 until exps.size) {
      if (i == exps.size - 1) return exps(i).execute(tempEnv)
      exps(i).execute(tempEnv)
    }
    null
  }
}