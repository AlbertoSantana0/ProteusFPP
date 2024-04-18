package ParsingTools

import TokenizationTools._
import TokenizationTools.FPPTokens._
import TokenizationTools.ProteusTokens._
import TokenizationTools.UnknownTokens._


object Parser {



  // function to parse through a list of tokens
  def parseTokens(tokenList: List[Token]): Unit = {
    var currentGroup: String = "Unknown"
    var bracketLevel = 0 // Counter for bracket nesting

    // Iterate through tokens
    tokenList.foreach { token =>
      token match {
        case _: Proteus => // Change to any Proteus token
          currentGroup = "Proteus"
        case OpenBracketToken =>
          bracketLevel += 1
        case CloseBracketToken =>
          bracketLevel -= 1
        case token: FPP =>
          currentGroup = "FPP"
        //case _: Proteus if bracketLevel > 0 => currentGroup = "Proteus"
        case _ =>
          currentGroup = "Unknown"
      }

      if (token != NewLineToken) {
        // Print the header of: Proteus or FPP when changing from FPP or Proteus
        if (currentGroup != "Unknown" && currentGroup != lastGroup) {
          //println(currentGroup + ":")
          lastGroup = currentGroup
        } else if(currentGroup == "Unknown") {
          println(s" $token")
        }
        //println(s" $token")
      }
    }
  }




  def parse(tokenList: List[Token], proteusTokens: List[Token], fppTokens: List[Token]): (List[Token], List[Token]) = {
    if(tokenList.isEmpty){
      (proteusTokens, fppTokens)
    } else {
      tokenList match {

        case ConstantToken :: IdentifierToken(name) :: EqualsToken :: OpenBracketToken :: rest => {
          val (fppValues, remainingTokens) = GetTokensUntilClosingBracket(rest)
          val newFPPTokens = fppTokens ++ List(ConstantToken, IdentifierToken(name), EqualsToken, OpenBracketToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }

        case ConstantToken :: IdentifierToken(name) :: EqualsToken :: QuoteToken :: QuoteToken :: QuoteToken :: NewLineToken :: rest => {
          val (fppValues, remainingTokens) = GetTokensUntilQuotation(rest)
          val newFPPTokens = fppTokens ++ List(ConstantToken, IdentifierToken(name),EqualsToken, QuoteToken, QuoteToken, QuoteToken, NewLineToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
      }

        case ConstantToken :: IdentifierToken(name) :: EqualsToken :: rest => {
          val (constantValue, remainingTokens) = GetConstantValue(rest)
          val newFPPTokens = fppTokens ++ List(ConstantToken, IdentifierToken(name), EqualsToken) ++ constantValue
          parse(remainingTokens, proteusTokens, newFPPTokens)

        }

        case ArrayToken :: IdentifierToken(name) :: EqualsToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(ArrayToken, IdentifierToken(name), EqualsToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case PostAnnotationToken :: IdentifierToken(name) :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(PostAnnotationToken, IdentifierToken(name)) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case AsyncToken :: CommandToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(AsyncToken, CommandToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case AsyncToken :: InputToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(AsyncToken, InputToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case SyncToken :: InputToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(SyncToken, InputToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case GuardedToken :: InputToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(GuardedToken, InputToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case OutputToken :: PortToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(OutputToken, PortToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case PortToken :: IdentifierToken(name) :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(PortToken, IdentifierToken(name)) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case TypeToken :: IdentifierToken(name) :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(TypeToken, IdentifierToken(name)) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case LocateToken :: ConstantToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(LocateToken, ConstantToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case LocateToken :: InstanceToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(LocateToken, InstanceToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case LocateToken :: ComponentToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(LocateToken, ComponentToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case LocateToken :: PortToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(LocateToken, PortToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case LocateToken :: TopologyToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(LocateToken, TopologyToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case CommandToken :: RecvToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(CommandToken, RecvToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case CommandToken :: RegToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(CommandToken, RegToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case CommandToken :: RespToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(CommandToken, RespToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case EventToken :: PortToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(EventToken, PortToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case TextToken :: PortToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(TextToken, PortToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case ParamToken :: GetToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(ParamToken, GetToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case ParamToken :: SetToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(ParamToken, SetToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case TelemetryToken :: PortToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(TelemetryToken, PortToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case TimeToken :: GetToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(TimeToken, GetToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case AsyncToken :: ProductToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(AsyncToken, ProductToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case ProductToken :: RequestToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(ProductToken, RequestToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case ProductToken :: SendToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(ProductToken, SendToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case ProductToken :: RecordToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(ProductToken, RecordToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }

        case LocateToken :: TypeToken :: rest => {
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(LocateToken, TypeToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)
        }
        case ActorToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfProteus, restOfCode) = parseBraces(rest, 1, List.empty)
          val newProteusTokens: List[Token] = proteusTokens ++ List(ActorToken, IdentifierToken(name), OpenBraceToken) ++ restOfProteus
          parse(restOfCode, newProteusTokens, fppTokens)
        }
        case StateToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfProteus, restOfCode) = parseBraces(rest, 1, List.empty)
          val newProteusTokens: List[Token] = proteusTokens ++ List(StateToken, IdentifierToken(name), OpenBraceToken) ++ restOfProteus
          parse(restOfCode.tail, newProteusTokens, fppTokens)
        }
        case MonitorToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfProteus, restOfCode) = parseBraces(rest, 1, List.empty)
          val newProteusTokens: List[Token] = proteusTokens ++ List(MonitorToken, IdentifierToken(name), OpenBraceToken) ++ restOfProteus
          parse(restOfCode, newProteusTokens, fppTokens)
        }
        case ModuleToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfFPP, restOfCode) = parseBraces(rest, 1, List.empty)
          val newFPPTokens = fppTokens ++ List(ModuleToken, IdentifierToken(name), OpenBraceToken) ++ restOfFPP
          parse(restOfCode, proteusTokens, newFPPTokens)
        }
        case PassiveToken :: ComponentToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfFPP, restOfCode) = parseBraces(rest, 1, List.empty)
          val newFPPTokens = fppTokens ++ List(PassiveToken, ComponentToken, IdentifierToken(name), OpenBraceToken) ++ restOfFPP
          parse(restOfCode, proteusTokens, newFPPTokens)
        }
        case FuncToken :: IdentifierToken(name) :: OpenParenthesisToken :: CloseParenthesisToken :: OpenBraceToken :: rest => {
          val (restOfProteus, restOfCode) = parseBraces(rest, 1, List.empty)
          val newProteusTokens: List[Token] = proteusTokens ++ List(FuncToken, IdentifierToken(name), OpenParenthesisToken, CloseParenthesisToken ,OpenBraceToken) ++ restOfProteus
          parse(restOfCode, newProteusTokens, fppTokens)
        }

        case ActiveToken :: ComponentToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfFPP, restOfCode) = parseBraces(rest, 1, List.empty)
          val newFPPTokens = fppTokens ++ List(ActiveToken, ComponentToken, IdentifierToken(name), OpenBraceToken) ++ restOfFPP
          parse(restOfCode, proteusTokens, newFPPTokens)
        }
        case QueuedToken :: ComponentToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfFPP, restOfCode) = parseBraces(rest, 1, List.empty)
          val newFPPTokens = fppTokens ++ List(QueuedToken, ComponentToken, IdentifierToken(name), OpenBraceToken) ++ restOfFPP
          parse(restOfCode, proteusTokens, newFPPTokens)
        }
        case EnumToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfFPP, restOfCode) = parseBraces(rest, 1, List.empty)
          val newFPPTokens = fppTokens ++ List(EnumToken, IdentifierToken(name), OpenBraceToken) ++ restOfFPP
          parse(rest, proteusTokens, newFPPTokens)
        }
        case TopologyToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfFPP, restOfCode) = parseBraces(rest, 1, List.empty)
          val newFPPTokens = fppTokens ++ List(TopologyToken, IdentifierToken(name), OpenBraceToken) ++ restOfFPP
          parse(rest, proteusTokens, newFPPTokens)
        }
        case ConnectionsToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfFPP, restOfCode) = parseBraces(rest, 1, List.empty)
          val newFPPTokens = fppTokens ++ List(ConnectionsToken, IdentifierToken(name), OpenBraceToken) ++ restOfFPP
          parse(rest, proteusTokens, newFPPTokens)
        }
        case StructToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfFPP, restOfCode) = parseBraces(rest, 1, List.empty)
          val newFPPTokens = fppTokens ++ List(StructToken, IdentifierToken(name), OpenBraceToken) ++ restOfFPP
          parse(rest, proteusTokens, newFPPTokens)
        }
        case EventToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfProteus, restOfCode) = parseBraces(rest, 1, List.empty)
          val newProteusTokens = proteusTokens ++ List(EventToken, IdentifierToken(name), OpenBraceToken) ++ restOfProteus
          parse(restOfCode.tail, newProteusTokens :+ restOfCode.head, fppTokens)
        }


        case _ :: rest => parse(rest, proteusTokens, fppTokens)
        case Nil => (proteusTokens, fppTokens) // This takes care of the warning, where it catches any Nil cases

      }
    }

  }
  
//{}
  def parseBraces(tokenList: List[Token], bracketLevel: Int, currList: List[Token]): (List[Token], List[Token]) = {
    if(tokenList.isEmpty) {
      throw new Exception("Missing Closing Brace")
    } else {
      val newBracketLevel = tokenList.head match {
        case OpenBraceToken => bracketLevel + 1
        case CloseBraceToken => bracketLevel - 1
        case _ => bracketLevel
      }
      if (tokenList.head == CloseBraceToken && newBracketLevel == 0) {
        (currList :+ tokenList.head, tokenList.tail)
      } else {
        parseBraces(tokenList.tail, newBracketLevel, currList :+ tokenList.head)
      }
    }

  }

  def GetConstantValue(tokenList: List[Token]): (List[Token], List[Token]) = {
    @scala.annotation.tailrec
    def loop(remaining: List[Token], ConstantTokens: List[Token]): (List[Token], List[Token]) = {
      remaining match {
        case NewLineToken :: rest => (ConstantTokens :+ NewLineToken, rest)
        case head :: tail  => loop(tail, ConstantTokens :+ head) // Stop at either newline
        case _ => (ConstantTokens, remaining)
      }
    }

    loop(tokenList, List.empty)
  }


  def GetFppValues(tokenList: List[Token]): (List[Token], List[Token]) = {
    @scala.annotation.tailrec
    def loop(remaining: List[Token], ArrayTokens: List[Token]): (List[Token], List[Token]) = {
      remaining match {
        case NewLineToken :: rest => (ArrayTokens :+ NewLineToken, rest)
        case head :: tail => loop(tail, ArrayTokens :+ head) // Stop at either newline
        case _ => (ArrayTokens, remaining)
      }
    }

    loop(tokenList, List.empty)
  }

  def GetTokensUntilClosingBracket(tokenList: List[Token]): (List[Token], List[Token]) = {
    @scala.annotation.tailrec
    def loop(remaining: List[Token], fppValues: List[Token], BracketLevel :Int): (List[Token], List[Token]) = {
      remaining match {
        case CloseBracketToken :: NewLineToken :: rest => {
          if (BracketLevel-1 == 0) {
            (fppValues :+ CloseBracketToken :+ NewLineToken, rest)
          } else {loop(rest, fppValues :+ CloseBracketToken :+ NewLineToken, BracketLevel-1)}
        }
        case CloseBracketToken :: rest => {
          if (BracketLevel-1 == 0) {
            (fppValues :+ CloseBracketToken, rest)
          } else {loop (rest, fppValues :+ CloseBracketToken, BracketLevel-1 )}
        }
        case OpenBracketToken :: rest => loop(rest, fppValues :+ OpenBracketToken, BracketLevel+1)
        case NewLineToken :: rest => loop(rest, fppValues :+ NewLineToken, BracketLevel)
        case head :: tail => loop(tail, fppValues :+ head, BracketLevel)
        case Nil => throw new Exception("Missing Closing Bracket")
      }
    }

    loop(tokenList, List.empty, 1)
  }

  def GetTokensUntilQuotation(tokenList: List[Token]): (List[Token], List[Token]) = {
    @scala.annotation.tailrec
    def loop(remaining: List[Token], fppValues: List[Token]): (List[Token], List[Token]) = {
      remaining match {
        case QuoteToken :: QuoteToken :: QuoteToken :: NewLineToken :: rest => (fppValues :+ QuoteToken :+ QuoteToken :+ QuoteToken :+ NewLineToken, rest)
        case QuoteToken :: QuoteToken :: QuoteToken :: rest => (fppValues :+ QuoteToken :+ QuoteToken :+ QuoteToken, rest)
        case NewLineToken :: rest => loop(rest, fppValues :+ NewLineToken)
        case head :: tail => loop(tail, fppValues :+ head)
        case Nil => throw new Exception("Missing set of Triple Quotations")
      }
    }

    loop(tokenList, List.empty)

  }



  private var lastGroup: String = _

  def main(args: Array[String]): Unit = {

    // input
    val tokenList = List(
      EventToken, IdentifierToken("EVENTLOL"), OpenBraceToken, BoolToken, CloseBraceToken, SemicolonToken,

    )
    //parseTokens(tokenList)
    val (proteus, fpp) = parse(tokenList, List.empty, List.empty)
    print("THIS IS PROTEUS \n")
    print(proteus)
    print("\n\n")
    print("THIS IS FPP \n")
    print(fpp)
  }
}