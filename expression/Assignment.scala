package expression

import context.Environment
import value._
import context.TypeException

case class Assignment(var vbl: Identifier, var update: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    if(env(vbl).isInstanceOf[Variable]) {
      env(vbl).asInstanceOf[Variable].content = update.execute(env)    
      Notification.DONE
    }
    else throw new TypeException("Contents of vbl must be of type Variable")
  }
}