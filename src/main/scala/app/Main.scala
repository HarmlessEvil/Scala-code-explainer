package app

import scala.meta._

object Main {
  def main(args: Array[String]): Unit = {
    val path = java.nio.file.Paths.get("tests", "12.scala")
    val sourceCode = new SourceCodeReader(path)

    sourceCode.tree.traverse {
      case node => SourceCodePrinter.printNode(node)
    }
  }
}
