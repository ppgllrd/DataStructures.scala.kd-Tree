package point

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
    val x = xs(i)
    val y = ys(i)
    math.sqrt(x*x + y*y)
  }
}
