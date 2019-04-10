package app

import scala.meta._

class SourceCodeReader(private val path: java.nio.file.Path) {
  private val bytes = java.nio.file.Files.readAllBytes(path)
  private val text = new String(bytes, "UTF-8")
  private val input = Input.VirtualFile(path.toString, text)

  val tree = input.parse[Source].get
}