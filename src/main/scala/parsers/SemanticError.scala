package parsers

case class SemanticError(errorMessage: String) extends Throwable {
  override def toString: String = errorMessage
}
