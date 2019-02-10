package point

import scala.util.Random

object ArrayPointSet {
  def apply(rnd : Random, size : Int, min : Int, max : Int) : ArrayPointSet = {
    val range = max-min+1
    val xs = Array.fill[Int](size)(min+rnd.nextInt(range))
    val ys = Array.fill[Int](size)(min+rnd.nextInt(range))
    new ArrayPointSet(xs, ys)
  }

  def apply(xs : Array[Int], ys : Array[Int]) : ArrayPointSet = {
    new ArrayPointSet(xs, ys)
  }
}

class ArrayPointSet(val xs : Array[Int], val ys : Array[Int]) extends PointSet {
  require(xs.length == ys.length, "arrays for x and y coordinates should have same length")

  override val size: Int = xs.length

  override def x(i: Int): Int = xs(i) // assumes i is within range. Doesn't check for efficiency's sake

  override def y(i: Int): Int = ys(i) // assumes i is within range. Doesn't check for efficiency's sake

  // assumes i is within range. Doesn't check for efficiency's sake
  override def coord(i: Int, coord: Coordinate.Value): Int = coord match {
    case Coordinate.x => xs(i)
    case Coordinate.y => ys(i)
  }

  // assumes i and j are within range. Doesn't check for efficiency's sake
  override def distance(i: Int, j: Int): Double = {
    val dx = xs(i) - xs(j)
    val dy = ys(i) - ys(j)
    math.sqrt(dx*dx + dy*dy)
  }
}
