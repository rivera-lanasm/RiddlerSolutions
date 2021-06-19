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


  def test_intersection(pair) = {
    // test if p1_0 is between 
    val c00: bool = (p1)
    val c00: bool = (p1)
    val c0 
    val c00: bool = (p1)
    val c00: bool = (p1)
    val c1
    c1 or c0
  }

  def run_slicer(num: Int) = {
    // create random set of 2n points on unit circle: [0,2pi]
    val rand_points = create_points(num = num)
    // draw n lines, through each consecutive pair of points
    val rand_slices = rand_points.grouped(2).toList
    // list each pair of lines
    val pair_slices = rand_slices.combinations(2).toList
    // evaluate intersection of each pair of slices, numbero of intersections
    filter(x=>x).size
  }


  def main() = {  
    println("my name is miguel")
    println(create_points(2))
  }

  main()

}




