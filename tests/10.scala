object IdFactory {
  private var counter = 0

  def create(): Int = {
    counter += 1

    counter
  }
}

object Obj extends App {
  val newId: Int = IdFactory.create()
  println(newId)
  val newerId: Int = IdFactory.create()
  println(newerId)
}
