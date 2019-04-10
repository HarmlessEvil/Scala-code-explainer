case class Point(x: Int, y: Int)

object CaseClasses extends App {
  val point = Point(1, 2)
  val anotherPoint = Point(1, 2)
  val yetAnotherPoint = Point(2, 2)

  println(s"$point and $anotherPoint are " + (if (point == anotherPoint) "the same" else "different"))
  println(s"$point and $yetAnotherPoint are " + (if (point == yetAnotherPoint) "the same" else "different")) 
}