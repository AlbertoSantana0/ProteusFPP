package TokenizationTools


sealed trait Token

sealed trait FPP extends Token
sealed trait Proteus extends Token
sealed trait Unknown extends Token

// FPP Tokens
object FPPTokens{

  // FPP Reserved Words
  case object ActiveToken extends FPP
  case object ActivityToken extends FPP
  case object AlwaysToken extends FPP
  case object ArrayToken extends FPP
  case object AssertToken extends FPP
  case object AsyncToken extends FPP
  case object AtToken extends FPP
  case object BaseToken extends FPP
  case object BlockToken extends FPP
  case object ChangeToken extends FPP
  case object CommandToken extends FPP
  case object ComponentToken extends FPP
  case object ConnectionsToken extends FPP
  case object ConstantToken extends FPP
  case object ContainerToken extends FPP
  case object CpuToken extends FPP
  case object DefaultToken extends FPP
  case object DiagnosticToken extends FPP
  case object DropToken extends FPP
  case object EnumToken extends FPP
  case object FalseToken extends FPP
  case object FatalToken extends FPP
  case object FormatToken extends FPP
  case object GetToken extends FPP
  case object GuardedToken extends FPP
  case object HealthToken extends FPP
  case class HexadecimalToken(value: String) extends FPP
  case object HighToken extends FPP
  case object IdToken extends FPP
  case object ImportToken extends FPP
  case object IncludeToken extends FPP
  case object InputToken extends FPP
  case object InstanceToken extends FPP
  case object InternalToken extends FPP
  case object LocateToken extends FPP
  case object LowToken extends FPP
  case object MatchToken extends FPP
  case object ModuleToken extends FPP
  case object OpcodeToken extends FPP
  case object OrangeToken extends FPP
  case object OutputToken extends FPP
  case object ParamToken extends FPP
  case object PassiveToken extends FPP
  case object PhaseToken extends FPP
  case object PortToken extends FPP
  case object PriorityToken extends FPP
  case object PrivateToken extends FPP
  case object ProductToken extends FPP
  case object QueueToken extends FPP
  case object QueuedToken extends FPP
  case object RecordToken extends FPP
  case object RecvToken extends FPP
  case object RedToken extends FPP
  case object RefToken extends FPP
  case object RegToken extends FPP
  case object RequestToken extends FPP
  case object RespToken extends FPP
  case object SaveToken extends FPP
  case object SendToken extends FPP
  case object SerialToken extends FPP
  case object SetToken extends FPP
  case object SeverityToken extends FPP
  case object SizeToken extends FPP
  case object StackToken extends FPP
  case object StructToken extends FPP
  case object SyncToken extends FPP
  case object TelemetryToken extends FPP
  case object TextToken extends FPP
  case object ThrottleToken extends FPP
  case object TimeToken extends FPP
  case object TopologyToken extends FPP
  case object TrueToken extends FPP
  case object TypeToken extends FPP
  case object UpdateToken extends FPP
  case object WarningToken extends FPP
  case object WithToken extends FPP
  case object YellowToken extends FPP
  case object F32Token extends FPP
  case object F64Token extends FPP
  case object I16Token extends FPP
  case object I32Token extends FPP
  case object I64Token extends FPP
  case object I8Token extends FPP
  case object U16Token extends FPP
  case object U32Token extends FPP
  case object U64Token extends FPP
  case object U8Token extends FPP

  // FPP Operators
  case object DotToken extends FPP // .
  case object ColonToken extends FPP // :
  case object OpenBracketToken extends FPP // [
  case object CloseBracketToken extends FPP // ]
  case object AnnotationToken extends FPP // @
  case object PostAnnotationToken extends FPP // @<
  case object HashToken extends FPP // #

  def description: String = s"FPP: 'Token'"
}

// Proteus Tokens
object ProteusTokens{

  // Proteus Reserved Words
  case object ActorToken extends Proteus
  case object ActorNameToken extends Proteus
  case object ConstToken extends Proteus
  case object ElseToken extends Proteus
  case object EntryToken extends Proteus
  case object EventNameToken extends Proteus
  case object ExitToken extends Proteus
  case object FuncToken extends Proteus
  case object GoToken extends Proteus
  case object GoIfToken extends Proteus
  case object IfToken extends Proteus
  case object InitialToken extends Proteus
  case object PrintToken extends Proteus
  case object PrintLnToken extends Proteus
  case object ReturnToken extends Proteus
  case object StateToken extends Proteus
  case object StateMachineToken extends Proteus
  case object StateNameToken extends Proteus
  case object WhileToken extends Proteus
  case object MonitorToken extends Proteus

  // Proteus Operators
  case object DoubleSlashToken extends Proteus // //
  case object ModulusEqualToken extends Proteus // %=
  case object ModulusToken extends Proteus // %
  case object LeftShiftEqualToken extends Proteus // <<=
  case object RightShiftEqualToken extends Proteus // >>=
  case object LeftShiftToken extends Proteus // <<
  case object RightShiftToken extends Proteus // >>
  case object LessThanOrEqualToken extends Proteus // <=
  case object GreaterThanOrEqualToken extends Proteus // >=
  case object LessThanToken extends Proteus // <
  case object GreaterThanToken extends Proteus // >
  case object EqualToToken extends Proteus // ==
  case object NotEqualToken extends Proteus // !=
  case object LogicalNotToken extends Proteus // !
  case object BitwiseXorEqualToken extends Proteus // ^=
  case object BitwiseXorToken extends Proteus // ^
  case object LogicalAndToken extends Proteus // &&
  case object LogicalOrToken extends Proteus // ||
  case object TimesEqualToken extends Proteus // *=
  case object DivideEqualToken extends Proteus // /=
  case object PlusEqualToken extends Proteus // +=
  case object MinusEqualToken extends Proteus // -=

  def description: String = s"Proteus: 'Token'"
}

// Unknown Keywords
object UnknownTokens {
  // Data Types
  case class ValueToken(value: Int) extends Unknown
  case object TrueToken extends Unknown
  case object FalseToken extends Unknown

  // Identifier Token
  case class IdentifierToken(value: String) extends Unknown

  // Shared Reserved Words
  case object BoolToken extends Unknown
  case object EventToken extends Unknown
  case object IntToken extends Unknown
  case object OnToken extends Unknown
  case object StringToken extends Unknown

  // Shared Operators
  case object OpenParenthesisToken extends Unknown // (
  case object CloseParenthesisToken extends Unknown // )
  case object AsteriskToken extends Unknown // *
  case object PlusToken extends Unknown // +
  case object CommaToken extends Unknown // ,
  case object ArrowToken extends Unknown // ->
  case object MinusToken extends Unknown // -
  case object SlashToken extends Unknown // /
  case object BackslashToken extends Unknown // \
  case object SemicolonToken extends Unknown // ;
  case object EqualsToken extends Unknown // =
  case object OpenBraceToken extends Unknown // {
  case object CloseBraceToken extends Unknown // }
  case object NewLineToken extends Unknown // \n
  case object QuoteToken extends Unknown // "

  def description: String = s"Unknown: 'Token'"
}