/** ****************************************************************************
  * Interface for immutable indexed set of points
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

  // lazy val instead of def cause we assume point set is immutable
  lazy val bounds: (Int, Int, Int, Int) = {
    var xMin = x(0)
    var xMax = x(0)
    var yMin = y(0)
    var yMax = y(0)
    for (i <- 1 until size) {
      val vx = x(i)
      val vy = y(i)

      if (vx < xMin)
        xMin = vx
      else if (vx > xMax)
        xMax = vx

      if (vy < yMin)
        yMin = vy
      else if (vy > yMax)
        yMax = vy
    }
    (xMin, yMin, xMax, yMax)
  }

  def paint(g2D: scala.swing.Graphics2D, canvas: java.awt.Component): Unit = {
    val wc = canvas.getWidth
    val hc = canvas.getHeight
    val dimc = wc min hc

    val (xMin, yMin, xMax, yMax) = bounds
    val w = xMax - xMin
    val h = yMax - yMin
    val dim = w max h

    val f = 0.95
    val sc = f * dimc / dim

    val r = 2.5
    val d = 2*r

    val offX = (wc-f*dimc)/2
    val offY = (hc-f*dimc)/2
    def point(x : Int, y : Int): Unit = {
      val e = new java.awt.geom.Ellipse2D.Double(
          offX+(x-xMin+(dim-w)/2)*sc-r
        , offY+(y-yMin+(dim-h)/2)*sc-r
        , d, d)
      g2D.fill(e)
    }

    g2D.setColor(java.awt.Color.black)
    for(i <- 0 until size)
      point(x(i), y(i))
  }
}