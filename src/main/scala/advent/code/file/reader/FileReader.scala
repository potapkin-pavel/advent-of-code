package advent.code.file.reader

import scala.io.BufferedSource

object FileReader {
  def getLines(path: String): List[String] = {
    val bufferedSource: BufferedSource = scala.io.Source.fromFile(path)
    val lines = bufferedSource.getLines().toList
    bufferedSource.close
    lines
  }
}
