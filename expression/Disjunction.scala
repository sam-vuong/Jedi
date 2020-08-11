package expression

import value._
import context._

case class Disjunction(val operands: List[Expression]) extends SpecialForm {

  def execute(env: Environment): Value = {
    var result = false;
    for (i <- 0 until operands.length) {
      if (result == true) return Boole(result)
      if (operands(i).execute(env).isInstanceOf[Boole]) {
        result = operands(i).execute(env).asInstanceOf[Boole].value
      }
      else throw new JediException("&& inputs must be Booles")
    }
    return Boole(result)
  }
  
  override def toString = {
    var result = if (operands == Nil) "" else operands.head.toString
    for(operand <- operands.tail) result = result + " || " + operand.toString()
    result
  }

  // etc.
}