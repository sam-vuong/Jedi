package value

// add pre-defined notifications OK, DONE, and UNSPECIFIED


class Notification(val message: String) extends Value {
  override def toString = message
}

object Notification {
  def apply(message: String) = new Notification(message)
  val OK = new Notification("OK")
  val DONE = new Notification("DONE")
  val UNSPECIFIED = new Notification("UNSPECIFIED")
}