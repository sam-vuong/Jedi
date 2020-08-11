package expression

import context._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  def execute(env: Environment): Value = {
    var args: List[Value] = null
    
    // Changes parameter passing method based on set flag
    if (flags.paramPassing == flags.BY_VALUE) args = operands.map(_.execute(env))
    else if (flags.paramPassing == flags.BY_NAME) args = operands.map(new Thunk(Nil, _, env))
    else args = operands.map(new Text(_))
    
    if (env.contains(operator)) {
      val test = env(operator)
      if (test.isInstanceOf[Closure]) {
        val res = test.asInstanceOf[Closure]
        res.apply(args, env)
      }
      else throw new TypeException("operator must be a closure")
    }
    else if (flags.paramPassing == flags.BY_VALUE) {
      alu.execute(operator, args)
    }
    else {
      alu.execute(operator, operands.map(_.execute(env)))
    }
  }
  
  override def toString = {
    var result = operator.toString + "("
    if (operands != Nil) {
      result = result + operands.head.toString
      for(operand <- operands.tail) {
        result = result + ", " + operand
      }
    }
    result = result + ")"
    result
  }
}