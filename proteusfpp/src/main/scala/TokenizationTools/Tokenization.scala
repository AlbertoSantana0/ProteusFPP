package TokenizationTools

import scala.util.matching.Regex
import scala.util.Either
import scala.io.Source

import java.io.{File, PrintWriter}

object Tokenizer {

  val reservedWordMap = Map(
    "active" -> FPPTokens.ActiveToken,
    "activity" -> FPPTokens.ActivityToken,
    "always" -> FPPTokens.AlwaysToken,
    "array" -> FPPTokens.ArrayToken,
    "assert" -> FPPTokens.AssertToken,
    "async" -> FPPTokens.AsyncToken,
    "at" -> FPPTokens.AtToken,
    "base" -> FPPTokens.BaseToken,
    "block" -> FPPTokens.BlockToken,
    "change" -> FPPTokens.ChangeToken,
    "command" -> FPPTokens.CommandToken,
    "component" -> FPPTokens.ComponentToken,
    "connections" -> FPPTokens.ConnectionsToken,
    "constant" -> FPPTokens.ConstantToken,
    "container" -> FPPTokens.ContainerToken,
    "cpu" -> FPPTokens.CpuToken,
    "default" -> FPPTokens.DefaultToken,
    "diagnostic" -> FPPTokens.DiagnosticToken,
    "drop" -> FPPTokens.DropToken,
    "enum" -> FPPTokens.EnumToken,
    "false" -> FPPTokens.FalseToken,
    "fatal" -> FPPTokens.FatalToken,
    "format" -> FPPTokens.FormatToken,
    "get" -> FPPTokens.GetToken,
    "guarded" -> FPPTokens.GuardedToken,
    "health" -> FPPTokens.HealthToken,
    "high" -> FPPTokens.HighToken,
    "id" -> FPPTokens.IdToken,
    "import" -> FPPTokens.ImportToken,
    "include" -> FPPTokens.IncludeToken,
    "input" -> FPPTokens.InputToken,
    "instance" -> FPPTokens.InstanceToken,
    "internal" -> FPPTokens.InternalToken,
    "locate" -> FPPTokens.LocateToken,
    "low" -> FPPTokens.LowToken,
    "match" -> FPPTokens.MatchToken,
    "module" -> FPPTokens.ModuleToken,
    "opcode" -> FPPTokens.OpcodeToken,
    "orange" -> FPPTokens.OrangeToken,
    "output" -> FPPTokens.OutputToken,
    "param" -> FPPTokens.ParamToken,
    "passive" -> FPPTokens.PassiveToken,
    "phase" -> FPPTokens.PhaseToken,
    "port" -> FPPTokens.PortToken,
    "priority" -> FPPTokens.PriorityToken,
    "private" -> FPPTokens.PrivateToken,
    "product" -> FPPTokens.ProductToken,
    "queue" -> FPPTokens.QueueToken,
    "queued" -> FPPTokens.QueuedToken,
    "record" -> FPPTokens.RecordToken,
    "recv" -> FPPTokens.RecvToken,
    "red" -> FPPTokens.RedToken,
    "ref" -> FPPTokens.RefToken,
    "reg" -> FPPTokens.RegToken,
    "request" -> FPPTokens.RequestToken,
    "resp" -> FPPTokens.RespToken,
    "save" -> FPPTokens.SaveToken,
    "send" -> FPPTokens.SendToken,
    "serial" -> FPPTokens.SerialToken,
    "set" -> FPPTokens.SetToken,
    "severity" -> FPPTokens.SeverityToken,
    "size" -> FPPTokens.SizeToken,
    "stack" -> FPPTokens.StackToken,
    "struct" -> FPPTokens.StructToken,
    "sync" -> FPPTokens.SyncToken,
    "telemetry" -> FPPTokens.TelemetryToken,
    "text" -> FPPTokens.TextToken,
    "throttle" -> FPPTokens.ThrottleToken,
    "time" -> FPPTokens.TimeToken,
    "topology" -> FPPTokens.TopologyToken,
    "true" -> FPPTokens.TrueToken,
    "type" -> FPPTokens.TypeToken,
    "update" -> FPPTokens.UpdateToken,
    "warning" -> FPPTokens.WarningToken,
    "with" -> FPPTokens.WithToken,
    "yellow" -> FPPTokens.YellowToken,
    "F32" -> FPPTokens.F32Token,
    "F64" -> FPPTokens.F64Token,
    "I16" -> FPPTokens.I16Token,
    "I32" -> FPPTokens.I32Token,
    "I64" -> FPPTokens.I64Token,
    "I8" -> FPPTokens.I8Token,
    "U16" -> FPPTokens.U16Token,
    "U32" -> FPPTokens.U32Token,
    "U64" -> FPPTokens.U64Token,
    "U8" -> FPPTokens.U8Token,
    "actor" -> ProteusTokens.ActorToken,
    "actorname" -> ProteusTokens.ActorNameToken,
    "const" -> ProteusTokens.ConstToken,
    "else" -> ProteusTokens.ElseToken,
    "entry" -> ProteusTokens.EntryToken,
    "eventname" -> ProteusTokens.EventNameToken,
    "exit" -> ProteusTokens.ExitToken,
    "func" -> ProteusTokens.FuncToken,
    "go" -> ProteusTokens.GoToken,
    "goif" -> ProteusTokens.GoIfToken,
    "if" -> ProteusTokens.IfToken,
    "initial" -> ProteusTokens.InitialToken,
    "print" -> ProteusTokens.PrintToken,
    "println" -> ProteusTokens.PrintLnToken,
    "return" -> ProteusTokens.ReturnToken,
    "state" -> ProteusTokens.StateToken,
    "statemachine" -> ProteusTokens.StateMachineToken,
    "statename" -> ProteusTokens.StateNameToken,
    "while" -> ProteusTokens.WhileToken,
    "bool" -> UnknownTokens.BoolToken,
    "event" -> UnknownTokens.EventToken,
    "int" -> UnknownTokens.IntToken,
    "on" -> UnknownTokens.OnToken,
    "string" -> UnknownTokens.StringToken,
    "monitor" -> ProteusTokens.MonitorToken

  )

  def tokenize(source: List[Char]): List[Token] = {
    RecTokenize(source, List.empty[Token])
  }

  // @tailrec
  def RecTokenize(input: List[Char], accum: List[Token]): List[Token] = {
    val (_, inputWithoutLeadingSpaces) = takeWhileAndGetAfter(input)(_.isWhitespace)
    val (newInput) = skipWhitespaceAndComments(input)

    if (newInput.isEmpty) {
      accum
    }
    else {
      val (token, rest) = readToken(newInput)
      RecTokenize(rest, accum :+ token)
    }
  }

  def readToken(input: List[Char]): (Token, List[Char]) = {
    tokenizeSymbol(input)
      .orElse(tokenizeInt(input))
      .orElse(tokenizeWord(input))
      .getOrElse(throw new IllegalArgumentException(input.mkString))
  }

  def tokenizeSymbol(input: List[Char]): Option[(Token, List[Char])] = {
    input match {
      case '/' :: '/' :: rest => Some((ProteusTokens.DoubleSlashToken), rest)
      case '%' :: '=' :: rest => Some((ProteusTokens.ModulusEqualToken), rest)
      case '%' :: rest => Some((ProteusTokens.ModulusToken), rest)
      case '<' :: '<' :: '=' :: rest => Some((ProteusTokens.LeftShiftEqualToken), rest)
      case '>' :: '>' :: '=' :: rest => Some((ProteusTokens.RightShiftEqualToken), rest)
      case '<' :: '<' :: rest => Some((ProteusTokens.LeftShiftToken), rest)
      case '>' :: '>' :: rest => Some((ProteusTokens.RightShiftToken), rest)
      case '<' :: '=' :: rest => Some((ProteusTokens.LessThanOrEqualToken), rest)
      case '>' :: '=' :: rest => Some((ProteusTokens.GreaterThanOrEqualToken), rest)
      case '<' :: rest => Some((ProteusTokens.LessThanToken), rest)
      case '>' :: rest => Some((ProteusTokens.GreaterThanToken), rest)
      case '=' :: '=' :: rest => Some((ProteusTokens.EqualToToken), rest)
      case '!' :: '=' :: rest => Some((ProteusTokens.NotEqualToken), rest)
      case '!' :: rest => Some((ProteusTokens.LogicalNotToken), rest)
      case '^' :: '=' :: rest => Some((ProteusTokens.BitwiseXorEqualToken), rest)
      case '^' :: rest => Some((ProteusTokens.BitwiseXorToken), rest)
      case '&' :: '&' :: rest => Some((ProteusTokens.LogicalAndToken), rest)
      case '|' :: '|' :: rest => Some((ProteusTokens.LogicalOrToken), rest)
      case '*' :: '=' :: rest => Some((ProteusTokens.TimesEqualToken), rest)
      case '/' :: '=' :: rest => Some((ProteusTokens.DivideEqualToken), rest)
      case '+' :: '=' :: rest => Some((ProteusTokens.PlusEqualToken), rest)
      case '-' :: '=' :: rest => Some((ProteusTokens.MinusEqualToken), rest)
      case '.' :: rest => Some((FPPTokens.DotToken), rest)
      case ':' :: rest => Some((FPPTokens.ColonToken), rest)
      case '[' :: rest => Some((FPPTokens.OpenBracketToken), rest)
      case ']' :: rest => Some((FPPTokens.CloseBracketToken), rest)
      case '@' :: '<' :: rest => Some((FPPTokens.AnnotationToken), rest)
      case '@' :: rest => Some((FPPTokens.PostAnnotationToken), rest)
      case '#' :: rest => Some((FPPTokens.HashToken), rest)
      case '(' :: rest => Some((UnknownTokens.OpenParenthesisToken), rest)
      case ')' :: rest => Some((UnknownTokens.CloseParenthesisToken), rest)
      case '*' :: rest => Some((UnknownTokens.AsteriskToken), rest)
      case '+' :: rest => Some((UnknownTokens.PlusToken), rest)
      case ',' :: rest => Some((UnknownTokens.CommaToken), rest)
      case '-' :: '>' :: rest => Some((UnknownTokens.ArrowToken), rest)
      case '-' :: rest => Some((UnknownTokens.MinusToken), rest)
      case '/' :: rest => Some((UnknownTokens.SlashToken), rest)
      case ';' :: rest => Some((UnknownTokens.SemicolonToken), rest)
      case '=' :: rest => Some((UnknownTokens.EqualsToken), rest)
      case '{' :: rest => Some((UnknownTokens.OpenBraceToken), rest)
      case '}' :: rest => Some((UnknownTokens.CloseBraceToken), rest)
      case '"' :: rest => Some((UnknownTokens.QuoteToken), rest)
      case '\\' :: 'n' :: rest => Some((UnknownTokens.NewLineToken), rest)
      case '\\' :: rest => Some((UnknownTokens.BackslashToken), rest)
      case _           => None
    }
  }

  def tokenizeInt(input: List[Char]): Option[(Token, List[Char])] = {
    val (num, rest) = takeWhileAndGetAfter(input)(_.isDigit)
    if (num.nonEmpty) {
      rest match {
        case 'x' :: other  => tokenizeHex(other)
        case c :: other if c.isLetter => throw IllegalArgumentException("Invalid token, letter following digit")
        case _ => Some((UnknownTokens.ValueToken(num.mkString.toInt), rest))
      }
    } else {
      None
    }
  }

  def tokenizeHex(input: List[Char]): Option[(FPPTokens.HexadecimalToken, List[Char])] = {
    val (num, rest) = takeWhileAndGetAfter(input)(_.isDigit)
    if (num.nonEmpty) {
      rest match {
        case _ => Some((FPPTokens.HexadecimalToken("0x" + num.mkString), rest))
      }
    } else {
      None
    }
  }

  def tokenizeWord(input: List[Char]): Option[(Token, List[Char])] = {
    input match {
      case c :: rest if c.isLetter || c == '_' || c.isDigit => {
        val (wrd, rest) = takeWhileAndGetAfter(input)(a => a.isLetterOrDigit || a == '_')
        val word = wrd.mkString
        val token = reservedWordMap.get(word).getOrElse(UnknownTokens.IdentifierToken(word))
        Some((token, rest))
      }
      case _ => None
    }
  }

  def takeWhileAndGetAfter[A](input: List[A])(check: (A) => Boolean): (List[A], List[A]) = {
    val take = input.takeWhile(check)
    (take, input.drop(take.size))
  }

  def skipWhitespaceAndComments(ch: List[Char]): List[Char] = {
    ch match {
      case '\n' :: tail => '\\' :: 'n' :: skipWhitespaceAndComments(tail)
      case '/' :: '/' :: tail => skipWhitespaceAndComments(tail.dropWhile(_ != '\n'))
      case '#' :: tail => skipWhitespaceAndComments(tail.dropWhile(_ != '\n'))
      case c :: tail if c.isWhitespace => skipWhitespaceAndComments(tail)
      case _ => ch
    }
  }

  def getParentTypeName(token: Token): String = token match {
    case _: FPP => "FPP"
    case _: Proteus => "Proteus"
    case _: Unknown => "Unknown"
    case null => "Not in Dictionary"
  }

  def tokenType(tokens: List[Token]): String = {
    tokens.map { token =>
      val typeName = token.getClass.getSimpleName
      val parentTypeName = getParentTypeName(token)
      val value = token match {
        case t: UnknownTokens.ValueToken => s"(value: ${t.value})"
        case t: UnknownTokens.IdentifierToken => s"(value: ${t.value})"
        case _ => ""
      }
      s"$parentTypeName($typeName$value)"
    }.mkString(", ")
  }
}

object TokenizationExample {
  def main(args: Array[String]): Unit = {
    val filePath = "testInputV3.txt"
    val fileContent = Source.fromFile(new File(filePath)).toList
    val tokens = Tokenizer.tokenize(fileContent)
    println(tokens)
    val tokenTypes = Tokenizer.tokenType(tokens)
    println("\n" + tokenTypes)
  }
}

// Create a dictionary for both languages
// fpp errors: \\n, 0x300
// .03f