package parsers

case class RuntimeException(errorMessage: String) extends Throwable {
  override def toString: String = errorMessage
}
