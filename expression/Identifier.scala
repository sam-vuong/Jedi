package expression
  
import context.Environment
import value.Value
import value.Thunk
import value.Text
import context.UndefinedException

case class Identifier(val name: String) extends Expression {
   override def toString = name
   def execute(env: Environment): Value = {
     // Checks Environment for this Identifier
     if (env.contains(this)) {
       val v = env(this)
       var res: Value = v
       if (v.isInstanceOf[Thunk]) {
         res = v.asInstanceOf[Thunk].apply()
       }
       else if (v.isInstanceOf[Text]) {
         res = v.asInstanceOf[Text].body.execute(env)
       }
       res
     }
     else throw new UndefinedException(this)
   }
}