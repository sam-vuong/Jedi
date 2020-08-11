package value

import expression.Literal 

// do the same for Boole, Real, ...
case class Integer(val value: Int) extends Literal with Ordered[Integer] with Equals {
  def +(other: Integer) = Integer(this.value + other.value)
  // *, -, /
  def -(other: Integer) = Integer(this.value - other.value)
  def *(other: Integer) = Integer(this.value * other.value)
  def /(other: Integer) = Integer(this.value / other.value)
  def unary_- = 0 - this.value // unary negation
  override def toString = value.toString
  def compare(other: Integer): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Integer]
  override def equals(other: Any): Boolean =
    other match {
       case other: Integer => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
}

// the following is added to the already created companion object
object Integer {
  // implicity called whenever an Integer is used wherever a Real is expected
  implicit def intToReal(n: Integer): Real = Real(n.value.toDouble)
}