package robopizza

import scala.math._
import scala.collection.immutable.ListMap
import com.github.tototoshi.csv._

object RoboSlicer extends App{

  def create_points(num: Int) = {
    assert(num > 1)
    val r = scala.util.Random
    def add_randomFl(l:List[AnyVal], num:Int): List[AnyVal] = {
      if (num > 0) add_randomFl(l:::List(r.nextFloat), num-1) else l
      }
    add_randomFl(List[AnyVal](), 2*num)
      .map(x => x.toString.toDouble )
      .map(x => (1.0-x)*0 + (x*2.0*math.Pi)) // map between 0 and 2*pi
    }


  def test_intersection(pair: List[List[Double]]) = {
    //pair 1
    val p1 = pair.head
    val p2 = pair.tail.head
    // test 1 for non-intersection
    val t1 = (p1.max < p2.max) & (p1.min > p2.min)
    // test 3 for non-intersection
    val t2 = (p1.max > p2.max) & (p1.min < p2.min)
    //combine
    val t = t1 || t2
    t
  }

  def run_slicer(num: Int) = {
    // create random set of 2n points on unit circle: [0,2pi]
    val rand_points = create_points(num = num)
    // draw n lines, through each consecutive pair of points
    val rand_slices = rand_points.grouped(2).toList
    // list each pair of lines
    val pair_slices = rand_slices.combinations(2).toList
    // evaluate intersection of each pair of slices, number of intersections
    pair_slices.map(x => test_intersection(x)).filter(x=>x).size
  }

  def write_output(order: String,output: List[Float], n: Int) = {
    // format list into 1-d array
    val output_write: List[List[Float]] = for (n <- output) yield {List(n)}
    // csv
    val n_out: String = n.toString
    val fileName: String = f"EmpiricalDistribution_N$n" + f"_Order$order" + ".csv"
    val f = new java.io.File(fileName)
    val writer = CSVWriter.open(f)
    writer.writeAll(output_write)
  }


  def main() = {  
    println("my name is miguel")
    val N: Int = 500
    val cuts :Int = 3
    // run simulation
    val output: List[Int] = (for {i <- 0 until N} yield run_slicer(cuts) + cuts + 1 ).toList
    println(output.sum / N)
    // convert output to relative frequency 
    val output_relative = output.groupBy(identity).map(t => (t._1, t._2.length.toFloat / output.size ))
    val output_relative_1 = ListMap(output_relative.toSeq.sortBy(_._1):_*).values.toList
    // write to csv 
    write_output(order=cuts.toString,output=output_relative_1, n = N)
  }

  main()
}




