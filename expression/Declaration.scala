package expression

import value._
import context._

case class Declaration(val id: Identifier, val init: Expression) extends SpecialForm {
  
   override def toString = "def " + id + " = " + init
   
   def execute(env: Environment): Value = {
     env.put(id, init.execute(env))
     Notification.OK
   }
}