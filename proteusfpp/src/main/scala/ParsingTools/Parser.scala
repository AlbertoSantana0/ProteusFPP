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


        case ConstantToken :: IdentifierToken(name) :: EqualsToken :: rest => {
          val (constantValue, remainingTokens) = GetConstantValue(rest)
          val newFPPTokens = fppTokens ++ List(ConstantToken, IdentifierToken(name), EqualsToken) ++ constantValue
          parse(remainingTokens, proteusTokens, newFPPTokens)

        }


        case ArrayToken :: IdentifierToken(name) :: EqualsToken :: rest =>
          val (fppValues, remainingTokens) = GetFppValues(rest) // New function to get FPP values
          val newFPPTokens = fppTokens ++ List(ArrayToken, IdentifierToken(name), EqualsToken) ++ fppValues
          parse(remainingTokens, proteusTokens, newFPPTokens)

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

        /*
        case MonitorToken :: IdentifierToken(name) :: OpenBraceToken :: rest => {
          val (restOfProteus, restOfCode) = parseBraces(rest, 1, List.empty)
          val newProteusTokens: List[Token] = proteusTokens ++ List(MonitorToken, IdentifierToken(name), OpenBraceToken) ++ restOfProteus
          parse(restOfCode, newProteusTokens, fppTokens)
        }
         */


        //add @ case, everything in between @ and NewLineToken



        //add async case


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

  private var lastGroup: String = _

  def main(args: Array[String]): Unit = {

    // input
    val tokenList = List(
      EventToken, IdentifierToken("EVENTLOL"), OpenBraceToken, BoolToken, CloseBraceToken, SemicolonToken,

      ActorToken, IdentifierToken("EventParameters"), OpenBraceToken, NewLineToken,
      StateMachineToken, OpenBraceToken, NewLineToken,
      EntryToken, OpenBraceToken, NewLineToken,
      CloseBraceToken, NewLineToken,
      CloseBraceToken, NewLineToken,
      CloseBraceToken, NewLineToken,

      PassiveToken, ComponentToken, IdentifierToken("EventParameters"), OpenBraceToken, NewLineToken,
      EventToken, PortToken, IdentifierToken("EventOut"), NewLineToken,
      TextToken, EventToken, PortToken, IdentifierToken("textEventOut"), NewLineToken,
      TimeToken, GetToken, PortToken, IdentifierToken("timeGetOut"), NewLineToken,
      EventToken, IdentifierToken("Event1"), OpenParenthesisToken, NewLineToken,
      CloseParenthesisToken, NewLineToken,
      SeverityToken, ActivityToken, HighToken, NewLineToken,
      FormatToken, StringToken, NewLineToken,
      CloseBraceToken, NewLineToken,

      ActorToken, IdentifierToken("EventParameters"), OpenBraceToken, NewLineToken,
      IntToken, IdentifierToken("X"), IdentifierToken("="), IdentifierToken("0"), SemicolonToken, NewLineToken,
      FuncToken, IdentifierToken("f"), OpenParenthesisToken, CloseParenthesisToken, OpenBraceToken, CloseBraceToken, NewLineToken,
      CloseBraceToken, NewLineToken,

      EnumToken, IdentifierToken("FppConstant_c"), OpenBraceToken, NewLineToken,
      IdentifierToken("c"),  IdentifierToken("="),  IdentifierToken("0"), NewLineToken,
      CloseBraceToken, SemicolonToken, NewLineToken,

      ModuleToken, IdentifierToken("M"), OpenBraceToken, NewLineToken,
      ConstantToken, IdentifierToken("a"), IdentifierToken("="), IdentifierToken("1"), NewLineToken,
      CloseBraceToken, NewLineToken,

      ActorToken, IdentifierToken("Testing"), OpenBraceToken, NewLineToken,
      IntToken, IdentifierToken("Y"), IdentifierToken("="), IdentifierToken("111"), NewLineToken,
      CloseBraceToken, SemicolonToken, NewLineToken,

      ConstantToken, IdentifierToken("ultimateAnswer"), EqualsToken, ValueToken(42), NewLineToken,

      StructToken, IdentifierToken("S"), OpenBraceToken, NewLineToken,
      IdentifierToken("X"), ColonToken, U32Token, NewLineToken,
      IdentifierToken("Y"), ColonToken, StringToken, NewLineToken,
      CloseBraceToken, NewLineToken,

      ArrayToken, IdentifierToken("array"), EqualsToken, OpenBracketToken, ValueToken(3), CloseBracketToken, U32Token, NewLineToken,

      ConstantToken, IdentifierToken("AAA"), EqualsToken,OpenBracketToken, ArrayToken, ValueToken(1),
      CommaToken, ValueToken(2), CommaToken, ValueToken(3), CommaToken, CloseBracketToken, NewLineToken,

      StateToken, IdentifierToken("SSS"), OpenBraceToken, NewLineToken,
      FuncToken, IdentifierToken("f"), OpenParenthesisToken, CloseParenthesisToken, OpenBraceToken, CloseBraceToken, NewLineToken,
      EntryToken, OpenBraceToken, IdentifierToken("f"), OpenParenthesisToken, CloseParenthesisToken, SemicolonToken, CloseBraceToken, NewLineToken,
      ExitToken, OpenBraceToken, IdentifierToken("f"), OpenParenthesisToken, CloseParenthesisToken, SemicolonToken, CloseBraceToken, NewLineToken,
      CloseBraceToken, NewLineToken


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