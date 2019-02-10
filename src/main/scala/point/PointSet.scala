/** ****************************************************************************
  * Interface for indexed set of points
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package point

// indexed set of points
trait PointSet {
  def size: Int // number of points

  def x(i: Int): Int // x-coordinate of i-th point
  def y(i: Int): Int // y-coordinate of i-th point

  def apply(i: Int): Point =
    Point(x(i), y(i))

  def coord(i: Int, coord: Axis.Value): Int // get either x or y coordinate for i-th point

  def distance(i: Int, j: Int): Double // distance between i-th and j-th points

  def distanceSqrd(i: Int, j: Int): Double // square of distance between i-th and j-th points

  def bounds() : (Int,Int,Int,Int) = {
    var xLeft = x(0)
    var xRight = x(0)
    var yTop = y(0)
    var yBottom = y(0)
    for(i <- 1 until size) {
      val vx = x(i)
      val vy = y(i)

      if (vx < xLeft)
        xLeft = vx
      else if (vx > xRight)
        xRight = vx

      if (vy < yTop)
        yTop = vy
      else if (vy > yBottom)
        yBottom = vy
    }
    (xLeft, yTop, xRight, yBottom)
  }
}
