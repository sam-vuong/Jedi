package context
import scala.util.parsing.combinator._

class SOPParsers extends RegexParsers {
  def expression: Parser[Any] = sum
  def sum: Parser[Any] = product ~ opt("+" ~ sum)
  def product: Parser[Any] = term ~ rep("*" ~ term)
  def term: Parser[Any] = number | "(" ~ expression ~ ")"
  def number: Parser[Any] = """0|[1-9][0-9]*(\.[0-9]+)?""".r
}