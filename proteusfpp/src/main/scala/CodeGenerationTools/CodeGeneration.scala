package CodeGenerationTools

import scala.io.Source
import scala.util.{Try, Using}
import scala.annotation.varargs
import java.io.{File, PrintWriter}
import java.io.PrintWriter
import java.io.File
import scala.io.Source
import scala.util.Try
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success}

import TokenizationTools._
import ParsingTools._

object SyntaxGenerator {
  val tokenToSyntax: Map[Token, String] = Map(
    // Proteus Reserved Words
    ProteusTokens.ActorToken -> "actor",
    ProteusTokens.ActorNameToken -> "actorname",
    ProteusTokens.ConstToken -> "const",
    ProteusTokens.ElseToken -> "else",
    ProteusTokens.IfToken -> "if",
    ProteusTokens.InitialToken -> "initial",
    ProteusTokens.PrintToken -> "print",
    ProteusTokens.PrintLnToken -> "println",
    ProteusTokens.ReturnToken -> "return",
    ProteusTokens.WhileToken -> "while",
    ProteusTokens.FuncToken -> "func",
    ProteusTokens.StateMachineToken -> "statemachine",
    ProteusTokens.StateNameToken -> "statename",
    ProteusTokens.StateToken -> "state",
    ProteusTokens.EventNameToken -> "eventname",
    ProteusTokens.EntryToken -> "entry",
    ProteusTokens.ExitToken -> "exit",
    ProteusTokens.GoToken -> "go",
    ProteusTokens.GoIfToken -> "goif",
    // FPP Reserved Words
    FPPTokens.EnumToken -> "enum",
    FPPTokens.StructToken -> "struct",
    FPPTokens.ActiveToken -> "active",
    FPPTokens.ActivityToken -> "activity",
    FPPTokens.AlwaysToken -> "always",
    FPPTokens.AssertToken -> "assert",
    FPPTokens.AsyncToken -> "async",
    FPPTokens.BaseToken -> "base",
    FPPTokens.ChangeToken -> "change",
    FPPTokens.CommandToken -> "command",
    FPPTokens.ComponentToken -> "component",
    FPPTokens.ConnectionsToken -> "connections",
    FPPTokens.ContainerToken -> "container",
    FPPTokens.CpuToken -> "cpu",
    FPPTokens.DefaultToken -> "default",
    FPPTokens.DiagnosticToken -> "diagnostic",
    FPPTokens.DropToken -> "drop",
    FPPTokens.FatalToken -> "fatal",
    FPPTokens.FormatToken -> "format",
    FPPTokens.GetToken -> "get",
    FPPTokens.GuardedToken -> "guarded",
    FPPTokens.HealthToken -> "health",
    FPPTokens.HighToken -> "high",
    FPPTokens.IdToken -> "id",
    FPPTokens.ImportToken -> "import",
    FPPTokens.IncludeToken -> "include",
    FPPTokens.InputToken -> "input",
    FPPTokens.InstanceToken -> "instance",
    FPPTokens.InternalToken -> "internal",
    FPPTokens.LocateToken -> "locate",
    FPPTokens.LowToken -> "low",
    FPPTokens.MatchToken -> "match",
    FPPTokens.ModuleToken -> "module",
    FPPTokens.OpcodeToken -> "opcode",
    FPPTokens.OrangeToken -> "orange",
    FPPTokens.OutputToken -> "output",
    FPPTokens.ParamToken -> "param",
    FPPTokens.PassiveToken -> "passive",
    FPPTokens.PhaseToken -> "phase",
    FPPTokens.PortToken -> "port",
    FPPTokens.PriorityToken -> "priority",
    FPPTokens.PrivateToken -> "private",
    FPPTokens.ProductToken -> "product",
    FPPTokens.QueueToken -> "queue",
    FPPTokens.QueuedToken -> "queued",
    FPPTokens.RecordToken -> "record",
    FPPTokens.RecvToken -> "recv",
    FPPTokens.RedToken -> "red",
    FPPTokens.RefToken -> "ref",
    FPPTokens.RegToken -> "reg",
    FPPTokens.RequestToken -> "request",
    FPPTokens.RespToken -> "resp",
    FPPTokens.SaveToken -> "save",
    FPPTokens.SendToken -> "send",
    FPPTokens.SerialToken -> "serial",
    FPPTokens.SetToken -> "set",
    FPPTokens.SeverityToken -> "severity",
    FPPTokens.SizeToken -> "size",
    FPPTokens.StackToken -> "stack",
    FPPTokens.SyncToken -> "sync",
    FPPTokens.TelemetryToken -> "telemetry",
    FPPTokens.TextToken -> "text",
    FPPTokens.ThrottleToken -> "throttle",
    FPPTokens.TimeToken -> "time",
    FPPTokens.TopologyToken -> "topology",
    FPPTokens.TypeToken -> "type",
    FPPTokens.UpdateToken -> "update",
    FPPTokens.WarningToken -> "warning",
    FPPTokens.WithToken -> "with",
    FPPTokens.YellowToken -> "yellow",
    FPPTokens.AtToken -> "at",
    FPPTokens.F32Token -> "f32",
    FPPTokens.F64Token -> "f64",
    FPPTokens.I16Token -> "i16",
    FPPTokens.I32Token -> "i32",
    FPPTokens.I64Token -> "i64",
    FPPTokens.I8Token -> "i8",
    FPPTokens.U16Token -> "u16",
    FPPTokens.U32Token -> "u32",
    FPPTokens.U64Token -> "u64",
    FPPTokens.U8Token -> "u8",

    // Data Types
    UnknownTokens.TrueToken -> "true",
    UnknownTokens.FalseToken -> "false",
    // Shared Reserved Words
    UnknownTokens.BoolToken -> "bool",
    UnknownTokens.EventToken -> "event",
    UnknownTokens.IntToken -> "int",
    UnknownTokens.OnToken -> "on",
    UnknownTokens.StringToken -> "string",
    // Shared Operators
    UnknownTokens.OpenParenthesisToken -> "(",
    UnknownTokens.CloseParenthesisToken -> ")",
    UnknownTokens.AsteriskToken -> "*",
    UnknownTokens.PlusToken -> "+",
    UnknownTokens.CommaToken -> ",",
    UnknownTokens.ArrowToken -> "->",
    UnknownTokens.MinusToken -> "-",
    UnknownTokens.SlashToken -> "/",
    UnknownTokens.SemicolonToken -> ";",
    UnknownTokens.EqualsToken -> "=",
    UnknownTokens.OpenBraceToken -> "{",
    UnknownTokens.CloseBraceToken -> "}",
    UnknownTokens.NewLineToken -> "\n",
    UnknownTokens.QuoteToken -> "\"",

    // FPP Operators
    FPPTokens.AnnotationToken -> "@",
    FPPTokens.DotToken -> ".",
    FPPTokens.ColonToken -> ":",
    FPPTokens.OpenBracketToken -> "[",
    FPPTokens.CloseBracketToken -> "]",
    FPPTokens.PostAnnotationToken -> "@<",
    FPPTokens.HashToken -> "#",

    // Proteus Operators
    ProteusTokens.DoubleSlashToken -> "//",
    ProteusTokens.ModulusEqualToken -> "%=",
    ProteusTokens.ModulusToken -> "%",
    ProteusTokens.ModulusEqualToken -> "%=",
    ProteusTokens.ModulusToken -> "%",
    ProteusTokens.LeftShiftEqualToken -> "<<=",
    ProteusTokens.RightShiftEqualToken -> ">>=",
    ProteusTokens.LeftShiftToken -> "<<",
    ProteusTokens.RightShiftToken -> ">>",
    ProteusTokens.LessThanOrEqualToken -> "<=",
    ProteusTokens.GreaterThanOrEqualToken -> ">=",
    ProteusTokens.LessThanToken -> "<",
    ProteusTokens.GreaterThanToken -> ">",
    ProteusTokens.EqualToToken -> "==",
    ProteusTokens.NotEqualToken -> "!=",
    ProteusTokens.LogicalNotToken -> "!",
    ProteusTokens.BitwiseXorEqualToken -> "^=",
    ProteusTokens.BitwiseXorToken -> "^",
    ProteusTokens.LogicalAndToken -> "&&",
    ProteusTokens.LogicalOrToken -> "||",
    ProteusTokens.TimesEqualToken -> "*=",
    ProteusTokens.DivideEqualToken -> "/=",
    ProteusTokens.PlusEqualToken -> "+=",
    ProteusTokens.MinusEqualToken -> "-=",
  )

  def generateFiles(tokens: List[Token], fileName: String): Unit = {
      val generate = tokens.map(token => token match {
        case UnknownTokens.IdentifierToken(value) => value
        case UnknownTokens.ValueToken(value) => value.toString
        case FPPTokens.HexadecimalToken(value) => value
        case _ => {
          tokenToSyntax.get(token) match {
            case Some(syntax) => syntax
            case None => ""}
        }
  }).mkString(" ")
      val translation = s"$generate"
      Using(new PrintWriter(fileName)) { writer =>
          writer.write(translation)} match {
          case Failure(exception) =>
              println(s"Error writing to file $fileName: ${exception.getMessage}")
          case Success(_) =>
              println(s"File $fileName generated successfully.")
      }
  }

package CodeGenerationTools

import scala.io.Source
import scala.util.{Try, Using}
import scala.annotation.varargs
import java.io.{File, PrintWriter}
import java.io.PrintWriter
import java.io.File
import scala.io.Source
import scala.util.Try
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success}

import TokenizationTools._
import ParsingTools._

object SyntaxGenerator {
  val tokenToSyntax: Map[Token, String] = Map(
    // Proteus Reserved Words
    ProteusTokens.ActorToken -> "actor",
    ProteusTokens.ActorNameToken -> "actorname",
    ProteusTokens.ConstToken -> "const",
    ProteusTokens.ElseToken -> "else",
    ProteusTokens.IfToken -> "if",
    ProteusTokens.InitialToken -> "initial",
    ProteusTokens.PrintToken -> "print",
    ProteusTokens.PrintLnToken -> "println",
    ProteusTokens.ReturnToken -> "return",
    ProteusTokens.WhileToken -> "while",
    ProteusTokens.FuncToken -> "func",
    ProteusTokens.StateMachineToken -> "statemachine",
    ProteusTokens.StateNameToken -> "statename",
    ProteusTokens.StateToken -> "state",
    ProteusTokens.EventNameToken -> "eventname",
    ProteusTokens.EntryToken -> "entry",
    ProteusTokens.ExitToken -> "exit",
    ProteusTokens.GoToken -> "go",
    ProteusTokens.GoIfToken -> "goif",
    // FPP Reserved Words
    FPPTokens.EnumToken -> "enum",
    FPPTokens.StructToken -> "struct",
    FPPTokens.ActiveToken -> "active",
    FPPTokens.ActivityToken -> "activity",
    FPPTokens.AlwaysToken -> "always",
    FPPTokens.AssertToken -> "assert",
    FPPTokens.AsyncToken -> "async",
    FPPTokens.BaseToken -> "base",
    FPPTokens.ChangeToken -> "change",
    FPPTokens.CommandToken -> "command",
    FPPTokens.ComponentToken -> "component",
    FPPTokens.ConnectionsToken -> "connections",
    FPPTokens.ContainerToken -> "container",
    FPPTokens.CpuToken -> "cpu",
    FPPTokens.DefaultToken -> "default",
    FPPTokens.DiagnosticToken -> "diagnostic",
    FPPTokens.DropToken -> "drop",
    FPPTokens.FatalToken -> "fatal",
    FPPTokens.FormatToken -> "format",
    FPPTokens.GetToken -> "get",
    FPPTokens.GuardedToken -> "guarded",
    FPPTokens.HealthToken -> "health",
    FPPTokens.HighToken -> "high",
    FPPTokens.IdToken -> "id",
    FPPTokens.ImportToken -> "import",
    FPPTokens.IncludeToken -> "include",
    FPPTokens.InputToken -> "input",
    FPPTokens.InstanceToken -> "instance",
    FPPTokens.InternalToken -> "internal",
    FPPTokens.LocateToken -> "locate",
    FPPTokens.LowToken -> "low",
    FPPTokens.MatchToken -> "match",
    FPPTokens.ModuleToken -> "module",
    FPPTokens.OpcodeToken -> "opcode",
    FPPTokens.OrangeToken -> "orange",
    FPPTokens.OutputToken -> "output",
    FPPTokens.ParamToken -> "param",
    FPPTokens.PassiveToken -> "passive",
    FPPTokens.PhaseToken -> "phase",
    FPPTokens.PortToken -> "port",
    FPPTokens.PriorityToken -> "priority",
    FPPTokens.PrivateToken -> "private",
    FPPTokens.ProductToken -> "product",
    FPPTokens.QueueToken -> "queue",
    FPPTokens.QueuedToken -> "queued",
    FPPTokens.RecordToken -> "record",
    FPPTokens.RecvToken -> "recv",
    FPPTokens.RedToken -> "red",
    FPPTokens.RefToken -> "ref",
    FPPTokens.RegToken -> "reg",
    FPPTokens.RequestToken -> "request",
    FPPTokens.RespToken -> "resp",
    FPPTokens.SaveToken -> "save",
    FPPTokens.SendToken -> "send",
    FPPTokens.SerialToken -> "serial",
    FPPTokens.SetToken -> "set",
    FPPTokens.SeverityToken -> "severity",
    FPPTokens.SizeToken -> "size",
    FPPTokens.StackToken -> "stack",
    FPPTokens.SyncToken -> "sync",
    FPPTokens.TelemetryToken -> "telemetry",
    FPPTokens.TextToken -> "text",
    FPPTokens.ThrottleToken -> "throttle",
    FPPTokens.TimeToken -> "time",
    FPPTokens.TopologyToken -> "topology",
    FPPTokens.TypeToken -> "type",
    FPPTokens.UpdateToken -> "update",
    FPPTokens.WarningToken -> "warning",
    FPPTokens.WithToken -> "with",
    FPPTokens.YellowToken -> "yellow",
    FPPTokens.AtToken -> "at",
    FPPTokens.F32Token -> "f32",
    FPPTokens.F64Token -> "f64",
    FPPTokens.I16Token -> "i16",
    FPPTokens.I32Token -> "i32",
    FPPTokens.I64Token -> "i64",
    FPPTokens.I8Token -> "i8",
    FPPTokens.U16Token -> "u16",
    FPPTokens.U32Token -> "u32",
    FPPTokens.U64Token -> "u64",
    FPPTokens.U8Token -> "u8",

    // Data Types
    UnknownTokens.TrueToken -> "true",
    UnknownTokens.FalseToken -> "false",
    // Shared Reserved Words
    UnknownTokens.BoolToken -> "bool",
    UnknownTokens.EventToken -> "event",
    UnknownTokens.IntToken -> "int",
    UnknownTokens.OnToken -> "on",
    UnknownTokens.StringToken -> "string",
    // Shared Operators
    UnknownTokens.OpenParenthesisToken -> "(",
    UnknownTokens.CloseParenthesisToken -> ")",
    UnknownTokens.AsteriskToken -> "*",
    UnknownTokens.PlusToken -> "+",
    UnknownTokens.CommaToken -> ",",
    UnknownTokens.ArrowToken -> "->",
    UnknownTokens.MinusToken -> "-",
    UnknownTokens.SlashToken -> "/",
    UnknownTokens.SemicolonToken -> ";",
    UnknownTokens.EqualsToken -> "=",
    UnknownTokens.OpenBraceToken -> "{",
    UnknownTokens.CloseBraceToken -> "}",
    UnknownTokens.NewLineToken -> "\n",
    UnknownTokens.QuoteToken -> "\"",

    // FPP Operators
    FPPTokens.AnnotationToken -> "@",
    FPPTokens.DotToken -> ".",
    FPPTokens.ColonToken -> ":",
    FPPTokens.OpenBracketToken -> "[",
    FPPTokens.CloseBracketToken -> "]",
    FPPTokens.PostAnnotationToken -> "@<",
    FPPTokens.HashToken -> "#",

    // Proteus Operators
    ProteusTokens.DoubleSlashToken -> "//",
    ProteusTokens.ModulusEqualToken -> "%=",
    ProteusTokens.ModulusToken -> "%",
    ProteusTokens.ModulusEqualToken -> "%=",
    ProteusTokens.ModulusToken -> "%",
    ProteusTokens.LeftShiftEqualToken -> "<<=",
    ProteusTokens.RightShiftEqualToken -> ">>=",
    ProteusTokens.LeftShiftToken -> "<<",
    ProteusTokens.RightShiftToken -> ">>",
    ProteusTokens.LessThanOrEqualToken -> "<=",
    ProteusTokens.GreaterThanOrEqualToken -> ">=",
    ProteusTokens.LessThanToken -> "<",
    ProteusTokens.GreaterThanToken -> ">",
    ProteusTokens.EqualToToken -> "==",
    ProteusTokens.NotEqualToken -> "!=",
    ProteusTokens.LogicalNotToken -> "!",
    ProteusTokens.BitwiseXorEqualToken -> "^=",
    ProteusTokens.BitwiseXorToken -> "^",
    ProteusTokens.LogicalAndToken -> "&&",
    ProteusTokens.LogicalOrToken -> "||",
    ProteusTokens.TimesEqualToken -> "*=",
    ProteusTokens.DivideEqualToken -> "/=",
    ProteusTokens.PlusEqualToken -> "+=",
    ProteusTokens.MinusEqualToken -> "-=",
  )

  def generateFiles(tokens: List[Token], fileName: String): Unit = {
      val generate = tokens.map(token => token match {
        case UnknownTokens.IdentifierToken(value) => value
        case UnknownTokens.ValueToken(value) => value.toString
        case FPPTokens.HexadecimalToken(value) => value
        case _ => {
          tokenToSyntax.get(token) match {
            case Some(syntax) => syntax
            case None => ""}
        }
  }).mkString(" ")
      val translation = s"$generate"
      Using(new PrintWriter(fileName)) { writer =>
          writer.write(translation)} match {
          case Failure(exception) =>
              println(s"Error writing to file $fileName: ${exception.getMessage}")
          case Success(_) =>
              println(s"File $fileName generated successfully.")
      }
  }

  def generateUniqueFolderName(inputFile: String): String = {
    val timestamp = LocalDateTime
    .now()
    .format(DateTimeFormatter.ofPattern("yyyy.MM.dd_HH.mm.ss"))
    s"Output Folder/$inputFile/$timestamp"
  }
}
}
