/**
 * Created with IntelliJ IDEA.
 * User: fran
 * Date: 2/24/13
 * Time: 8:28 PM
 * To change this template use File | Settings | File Templates.
 */
object Week2{

  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a == b || a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product(f)(a + 1, b)
  }


}
