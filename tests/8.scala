class Greeter(prefix: String, suffix: String) {
  def greet(name: String): Unit = println(s"${prefix}${name}${suffix}")
}

object Classes extends App {
  val greeter = new Greeter("Hello, ", "!")
  greeter greet "Scala developer"
}