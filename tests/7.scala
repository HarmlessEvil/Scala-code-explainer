object Methods {
  def main(args: Array[String]): Unit = {
   def addThenMultiply(x: Int, y: Int)(z: Int)(k: Int): Int = (x + y) * z - k
   def name: String = System.getProperty("user.name")
   println(s"Hello, $name!")

   def getSquaredString(input: Double): String  = {
     val square = input * input
     square toString
   }
  }
}