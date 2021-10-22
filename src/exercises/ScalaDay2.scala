package exercises

import scala.language.postfixOps

class ScalaDay2 {
  /**
   * Bai 1: Scala Training, date = 19/07/2021
   *
   * @param crypt    - Mảng 3 phần tử tạo thành từ những ký tự đc mapping bởi 1 số cho trước
   *                 exp: ["SEND", "MORE", "MONEY"]
   * @param solution - Mảng 2 chiều mapping ký tự chữ và ký tự số, exp:
   *                 [['O', '0'],
   *                 ['M', '1'],
   *                 ['Y', '2'],
   *                 ['E', '5'],
   *                 ['N', '6'],
   *                 ['D', '7'],
   *                 ['R', '8'],
   *                 ['S', '9']]
   * @return Kiểm tra ký tự khi mapping sang số có thỏa mãn công thức x + y = z
   *         -> 9567 + 1085 = 10652 thỏa mãn
   */
  def isValidCrypt(crypt: Array[String], solution: Array[Array[Char]]): Boolean = {
    val solMap = solution.map(x => x(0) -> x(1)).toMap // đổi sang dạng Map
    val cryptSub = crypt.map(_.map(solMap)) // convert string letter => string số từ Map
    cryptSub(0).toLong + cryptSub(1).toLong == cryptSub(2).toLong // x + y = z?
  }

  /**
   * Bai 2: Scala Training, date = 19/07/2021
   *
   * @param s - String for checking, which will contain all characters from `t`
   * @param t - String contains distinct char, exp: "abc"
   * @return Substring from s which have min length and contains all characters from `t`
   */
  def minSubstring(s: String, t: String): String = {
    val lst = for {
      i <- 0 until s.length
      j <- (i + 1) to s.length
      ss = s.substring(i, j) // get all substrings from s
      if t.forall(ss.toSet.contains) // filter substrings that contains all `t` chars
    } yield ss -> i // results with index
    lst.sortBy(a => (a._1.length, a._2)).headOption.map(_._1).getOrElse("") // sort and get first one to return
  }

  /**
   * Bai 2: Scala Training, date = 19/07/2021, solution 2
   *
   * @param s - String for checking, which will contain all characters from `t`
   * @param t - String contains distinct char, exp: "abc"
   * @return Substring from s which have min length and contains all characters from `t`
   */
  def minSubstring2(s: String, t: String): String = {
    (for {
      i <- t.length to s.length
      opt <- s.sliding(i).toStream
      if t.forall(opt.contains(_))
    } yield opt) take 1 head
  }
}
// Main App For Testing
//~~~~~~~~~~~~~~~~~~~~~
object ScalaDay2 extends App {
  case class Greeting(name: String)

  def greetingMe(greeting: String)(implicit g: Greeting): String = s"${greeting} ${g.name}"
//  implicit val me = Greeting("Binh")

  println(greetingMe("Hi")(Greeting("Long")))      // Hi Binh
}


