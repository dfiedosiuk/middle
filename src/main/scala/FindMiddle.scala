import scala.annotation.tailrec

object FindMiddle extends App {

  def findMiddleElement[T](xs: List[T]): Option[T] = {

    val myXS = if(xs.nonEmpty && xs.length != 1) {
      if (xs.length % 2 == 0) {
        xs.init
      } else xs
    } else xs

    @tailrec
    def loop(ys: List[T], acc: List[T], cnt: Int): Option[T] = {

      ys match {
        case head :: tail if (cnt%2 == 0 && ys.length != 1 ) => loop(tail, acc.tail, cnt + 1)
        case _ :: tail => loop(tail, acc, cnt + 1)
        case List(n) => acc.headOption
        case Nil => acc.headOption
      }
    }
    loop(myXS, myXS, 0)
  }


  val numbers1 = List(1,2,3,4,5)
  val numbers2 = List(1,2,3,4,5,6)
  val numbers3 = List.empty[Int]
  val numbers4 = List(1,2)
  val numbers5 = List(1)

  println(findMiddleElement(numbers1))
  println(findMiddleElement(numbers2))
  println(findMiddleElement(numbers3))
  println(findMiddleElement(numbers4))
  println(findMiddleElement(numbers5))




}
