import TokenizationTools._
import ParsingTools._
import CodeGenerationTools._
import CodeGenerationTools.SyntaxGenerator._
import scala.io.Source
import java.io.{File, PrintWriter}
import scala.util.{Try, Using}
import scala.annotation.varargs
import java.io.PrintWriter
import java.io.File
import scala.util.Try
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success}

object Main{
  def main(args: Array[String]): Unit = {
    val inputFile = args.headOption.getOrElse("defaultfile.txt")
    try {
      val fileContent = Source.fromFile(new File(inputFile)).toList
      val tokens = Tokenizer.tokenize(fileContent)
      println(tokens)

      val (proteus, fpp) = Parser.parse(tokens, List.empty, List.empty)
      print("THIS IS PROTEUS \n")
      print(proteus)
      print("\n\n")
      print("THIS IS FPP \n")
      print(fpp)
  

      val folderName = generateUniqueFolderName(inputFile)
      val folder = Paths.get(folderName)
      Files.createDirectories(folder)
      val proteusFileName = "proteusfile.proteus"
      val fppFileName = "fppfile.fpp"
      generateFiles(proteus, s"$folderName/$proteusFileName")
      generateFiles(fpp, s"$folderName/$fppFileName")

      println(s"Files generated and placed in folder $folderName successfully.")
        } catch {
      case e: Exception => println(s"Error reading file: ${e.getMessage}")
    }
  }
}
