package point

object Coordinate extends Enumeration {
  val x, y = Value
}

trait PointSet {
  def size : Int // number of points

  def x(i : Int) : Int // x-coordinate of i-th point
  def y(i : Int) : Int // y-coordinate of i-th point

  def coord(i : Int, coord : Coordinate.Value) : Int // get either x or y coordinate for i-th point

  def distance(i : Int, j : Int) : Double // distance between i-th and j-th points
}
