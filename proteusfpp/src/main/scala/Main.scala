import TokenizationTools._
import ParsingTools._
import scala.io.Source
import java.io.{File, PrintWriter}

@main def Main(): Unit =
  val filePath = "src/main/scala/testInputV2.txt"
  val fileContent = Source.fromFile(new File(filePath)).toList
  val tokens = Tokenizer.tokenize(fileContent)
  println(tokens)

  val (proteus, fpp) = Parser.parse(tokens, List.empty, List.empty)
  print("THIS IS PROTEUS \n")
  print(proteus)
  print("\n\n")
  print("THIS IS FPP \n")
  print(fpp)