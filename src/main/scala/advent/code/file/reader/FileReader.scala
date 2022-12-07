package advent.code.file.reader

import scala.io.BufferedSource

object FileReader {
  def getLines(path: String): List[String] = {
    val bufferedSource: BufferedSource = scala.io.Source.fromFile(path)
    val lines = bufferedSource.getLines().toList
    bufferedSource.close
    lines
  }

  def getListOfChars(path: String): List[Char] = {
    val bufferedSource: BufferedSource = scala.io.Source.fromFile(path)
    val listOfChars = bufferedSource.toList
    bufferedSource.close
    listOfChars
  }
}
