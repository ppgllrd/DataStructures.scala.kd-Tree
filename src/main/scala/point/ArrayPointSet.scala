/** ****************************************************************************
  * Implementations of indexed set of points
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package point

import scala.util.Random

object ArrayPointSet {
  def uniform(rnd: Random, size: Int, min: Int, max: Int): ArrayPointSet = {
    val xs = new Array[Int](size)
    val ys = new Array[Int](size)
    val pointSet = new ArrayPointSet(xs, ys)
    pointSet.randomUniform(rnd, min, max)
    pointSet
  }

  def normal(rnd: Random, size: Int, sigma: Double = 1): ArrayPointSet = {
    val xs = new Array[Int](size)
    val ys = new Array[Int](size)
    val pointSet = new ArrayPointSet(xs, ys)
    pointSet.randomNormal(rnd, sigma)
    pointSet
  }

  def apply(xs: Array[Int], ys: Array[Int]): ArrayPointSet = {
    new ArrayPointSet(xs, ys)
  }
}

class ArrayPointSet(val xs: Array[Int], val ys: Array[Int]) extends PointSet {
  require(xs.length == ys.length, "arrays for x and y coordinates should have same length")

  override val size: Int = xs.length

  override def x(i: Int): Int = xs(i) // assumes i is within range. Doesn't check for efficiency's sake

  override def y(i: Int): Int = ys(i) // assumes i is within range. Doesn't check for efficiency's sake

  // assumes i is within range. Doesn't check for efficiency's sake
  override def coord(i: Int, coord: Axis.Value): Int = coord match {
    case Axis.x => xs(i)
    case Axis.y => ys(i)
  }

  // assumes i and j are within range. Doesn't check for efficiency's sake
  override def distance(i: Int, j: Int): Double = {
    val dx = xs(i) - xs(j)
    val dy = ys(i) - ys(j)
    math.sqrt(dx * dx + dy * dy)
  }

  // assumes i and j are within range. Doesn't check for efficiency's sake
  override def distanceSqrd(i: Int, j: Int): Double = {
    val dx = xs(i) - xs(j)
    val dy = ys(i) - ys(j)
    dx * dx + dy * dy
  }

  def randomUniform(rnd: Random, min: Int, max: Int): Unit = {
    val range = max - min + 1
    for (i <- xs.indices) {
      xs(i) = min + rnd.nextInt(range)
      ys(i) = min + rnd.nextInt(range)
    }
  }

  def randomNormal(rnd: Random, sigma: Double = 1): Unit = {
    val mu = 0.0
    val resolution = 100000000
    for (i <- xs.indices) {
      xs(i) = (resolution * (mu + sigma * rnd.nextGaussian())).toInt
      ys(i) = (resolution * (mu + sigma * rnd.nextGaussian())).toInt
    }
  }
}


// This implementation uses a cache of previous computed distances to
// speed-up subsequent queries

object CachedArrayPointSet {
  def uniform(rnd: Random, size: Int, min: Int, max: Int): CachedArrayPointSet = {
    val xs = new Array[Int](size)
    val ys = new Array[Int](size)
    val pointSet = new CachedArrayPointSet(xs, ys)
    pointSet.randomUniform(rnd, min, max)
    pointSet
  }

  def normal(rnd: Random, size: Int, sigma: Double = 1): CachedArrayPointSet = {
    val xs = new Array[Int](size)
    val ys = new Array[Int](size)
    val pointSet = new CachedArrayPointSet(xs, ys)
    pointSet.randomNormal(rnd, sigma)
    pointSet
  }

  def apply(xs: Array[Int], ys: Array[Int]): CachedArrayPointSet = {
    new CachedArrayPointSet(xs, ys)
  }
}

class CachedArrayPointSet(xs: Array[Int], ys: Array[Int]) extends ArrayPointSet(xs, ys) {
  private val cacheSize = {
    var sz = 1
    while (sz < size)
      sz *= 2
    sz
  }

  private val cacheSig = Array.fill[Int](cacheSize)(-1)
  private val cacheVal = new Array[Double](cacheSize)

  override def distance(i: Int, j: Int): Double = {
    val sig = if (i <= j) i else j
    val hash = i ^ j
    if (cacheSig(hash) != sig) {
      cacheSig(hash) = sig
      cacheVal(hash) = super.distance(i, j)
    }
    cacheVal(hash)
  }
}
