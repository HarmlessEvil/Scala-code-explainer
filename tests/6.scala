object Functions extends App {
  val addOne = (x: Int) => x + 1
  println(addOne(1))

  val add = (x: Int, y: Int) => x + y
  println(add(1, 3))

  val getTheAnswer = () => 42
  println(getTheAnswer())
}