package robopizza

import scala.math._

object RoboSlicer extends App{

  def create_points(num: Int) = {
    assert(num > 1)
    val r = scala.util.Random
    def add_randomFl(l:List[AnyVal], num:Int): List[AnyVal] = {
      if (num > 0) add_randomFl(l:::List(r.nextFloat), num-1) else l
      }
    add_randomFl(List[AnyVal](), 2*num)
      .map(x => x.toString.toDouble )
      .map(x => (1.0-x)*0 + (x*2.0*math.Pi))
    }


  // def assign_labels()


  // def pair_points()


  // def count_intersections()


  def main() = {  
    println("my name is miguel")
    println(create_points(2))
  }

  main()

}




