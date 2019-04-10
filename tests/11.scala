trait Greeter {
  def greet(name: String): Unit
  def greetMe(name: String): Unit =
    println(s"Hello, $name!")
}

class DefaultGreeter extends Greeter

class CustomizableGreeter(prefix: String, postfix: String) extends Greeter {
  override def greet(name: String): Unit = {
    println(s"$prefix$name$postfix")
  }
}

object Trait extends App {
  val greeter = new DefaultGreeter()
  greeter.greet("Scala developer")

  val customGreeter = new CustomizableGreeter("How are you, ", "?")
  customGreeter.greet("Scala developer")
}