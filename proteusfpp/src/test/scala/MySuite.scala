// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import TokenizationTools._
import TokenizationTools.FPPTokens._
import TokenizationTools.ProteusTokens._
import TokenizationTools.UnknownTokens._
import ParsingTools._
import CodeGenerationTools._
import CodeGenerationTools.SyntaxGenerator._

class MySuite extends munit.FunSuite {
  test("Testing FPP simple constant expressions") {
    val obtained = Parser.parse(List(ConstantToken, IdentifierToken("a"), EqualsToken, ValueToken(1)), List.empty, List.empty)
    val expected = (List.empty, List(ConstantToken, IdentifierToken("a"), EqualsToken, ValueToken(1)))
    assertEquals(obtained, expected)
  }
  //rename the tests and make different tests
  test("Testing FPP simple constant expressions") {
    val obtained = Parser.parse(List(ConstantToken, IdentifierToken("a"), EqualsToken, ValueToken(1)), List.empty, List.empty)
    val expected = (List.empty, List(ConstantToken, IdentifierToken("a"), EqualsToken, ValueToken(1)))
    assertEquals(obtained, expected)
  }
}
