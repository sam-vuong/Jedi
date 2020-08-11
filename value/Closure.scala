package value

import expression._
import context._

case class Closure(val parameters: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value], callingEnv: Environment): Value = {
    var tempEnv = new Environment(defEnv)
    if (flags.staticScope == false) {
      tempEnv = new Environment(callingEnv)
    }
    // bind parameters to arguments
    tempEnv.bulkPut(parameters, args)
    body.execute(tempEnv)
  }
}