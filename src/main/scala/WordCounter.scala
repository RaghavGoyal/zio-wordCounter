import zio.Console.{getStrLn, printLine, putStrLn}
import zio._

import scala.io.Source

object WordCounter extends ZIOAppDefault {

  val startMsg = "Welcome to word counter... Enter the word you want to count"
  val wordCounterResourcePath = "/home/knoldus/Desktop/zio-wordCounter/src/main/resources/wordCounter"
  val filesList = List("file1.txt","file2.txt", "file3.txt", "file4.txt", "file5.txt")

  override def run: ZIO[zio.ZEnv with Has[ZIOAppArgs], Any, Any] = (for{
    wordsToSearch <- putStrLn(startMsg) *> getUserInput
    counts <- startOperation(wordsToSearch.head)
    _ <- putStrLn(s"word: ${wordsToSearch.head}; count: $counts")
  }yield()).exitCode

  def getUserInput ={
    for{
      input <- getStrLn
    }yield input.split(",").toList
  }

//  def startCounting(words: List[String]) = {
//    words.map(word => startOperation(word).fork)
//  }

  def startOperation(word: String) = {
    for{
      fibre1 <- countWordFromFile("file1.txt", word).fork
      fibre2 <- countWordFromFile("file2.txt", word).fork
      fibre3 <- countWordFromFile("file3.txt", word).fork
      fibre4 <- countWordFromFile("file4.txt", word).fork
      fibre5 <- countWordFromFile("file5.txt", word).fork
      count1 <- fibre1.join
      count2 <- fibre2.join
      count3 <- fibre3.join
      count4 <- fibre4.join
      count5 <- fibre5.join
    }yield count1 + count2 + count3 + count4 + count5
  }

  def countWordFromFile(fileName: String, word: String) ={
    val file = Source.fromFile(s"$wordCounterResourcePath/$fileName")
    ZIO.acquireReleaseWith(
      // acquire effect
      ZIO.attemptBlocking(file)
    )(
      // Release effect
      file => ZIO.attempt(file.close()).orDie
    ){ // usage effect
      file =>
        ZIO.attemptBlocking(
          file.getLines().mkString(" ").split(" ").count(_.equalsIgnoreCase(word))
      )
    }
  }
}
