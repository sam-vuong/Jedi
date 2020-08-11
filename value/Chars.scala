package value

import expression.Literal 

case class Chars(val value: String) extends Literal with Ordered[Chars] with Equals {
  def +(other: Chars) = Chars(this.value + other.value)
  def substring(start: Integer, end: Integer) = Chars(this.value.substring(start.value, end.value))
  override def toString = value.toString
  def compare(other: Chars): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Chars]
  override def equals(other: Any): Boolean =
    other match {
       case other: Chars => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
}