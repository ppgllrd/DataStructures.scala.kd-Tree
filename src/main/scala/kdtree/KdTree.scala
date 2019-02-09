/******************************************************************************
 * Semidynamic K-d Trees
 *
 * Pepe Gallardo, 2019
 *
 * See: K-d trees for semidynamic point sets
 *      Jon Louis Bentley
 *      SCG '90 Proceedings of the sixth annual symposium
 *      on Computational Geometry, 1990
 *
 *****************************************************************************/

package kdtree

import point.{Coordinate, PointSet}



case class KdTree(pointSet: PointSet, bucketSize : Int = 3) {
  val size : Int = pointSet.size

  protected val perms = new Array[Int](size) // permutation of points
  for(i <- perms.indices)
    perms(i) = i // start with identity permutation

  protected val buckets = new Array[Bucket](size)

  protected val root : KdNode = build(0, size-1, None)

  private def findMaxSpread(lo : Int, hi : Int) : Coordinate.Value = {
    val plo = perms(lo)
    var minX = pointSet.x(plo)
    var maxX = pointSet.x(plo)
    var minY = pointSet.y(plo)
    var maxY = pointSet.y(plo)

    var i = lo+1
    while(i <= hi) {
      val pi = perms(i)
      val x = pointSet.x(pi)
      val y = pointSet.y(pi)

      if(x < minX)
        minX = x
      else if(x > maxX)
        maxX = x

      if(y < minY)
        minY = y
      else if(y > maxY)
        maxY = y

      i += 1
    }
    if ((maxX - minX) >= (maxY - minY)) Coordinate.x else Coordinate.y
  }

  private def swap(i : Int, j : Int): Unit = {
    val tmp = perms(i)
    perms(i) = perms(j)
    perms(j) = tmp
  }

  /* Lomuto partition scheme.
   *
   * Returns final position of pivot.
   * Postcondition:
   *  Let p be final position of pivot (output of this function)
   *  Then, points in [lo,hi] range are permuted so that:
   *   points.x(perm(i)) <= points.x(perm(pivot)) forall i . lo  <= i < p
   *   points.x(perm(i)) >= points.x(perm(pivot)) forall i . p+1 <= i <= hi
   */
  private def partitionX(lo : Int, hi : Int, pivot : Int): Int = {
    val pivotVal = pointSet.x(perms(pivot))

    // swap elements at hi and pivot
    swap(hi, pivot)

    var p = lo
    for(i <- lo until hi) {
      if(pointSet.x(perms(i)) < pivotVal) {
        // swap elements at i and p
        swap(i, p)
        p += 1
      }
    }
    // move pivot to its final place
    swap(p, hi)
    p
  }

  /* point.Points in [lo,hi] range are permuted so that:
   *  points.x(perm(i)) <= points.x(perm(m)) forall i . lo  <= i < m
   *  points.x(perm(i)) >= points.x(perm(m)) forall i . m+1 <= i <= hi
   */
  private def selectX(lo0: Int, hi0: Int, m: Int): Unit = {
    var lo = lo0
    var hi = hi0
    var stop = lo == hi
    while(!stop) {
      val pivot = partitionX(lo, hi, lo) // third arg lo is pivot but anyone would do

      if(pivot == m)
        stop = true
      else if(m < pivot)
        hi = pivot-1
      else
        lo = pivot+1
    }
  }

  private def partitionY(lo : Int, hi : Int, pivot : Int): Int = {
    val pivotVal = pointSet.y(perms(pivot))

    // swap elements at hi and pivot
    swap(hi, pivot)

    var p = lo
    for(i <- lo until hi) {
      if(pointSet.y(perms(i)) < pivotVal) {
        // swap elements at i and p
        swap(i, p)
        p += 1
      }
    }
    // move pivot to its final place
    swap(p, hi)
    p
  }

  private def selectY(lo0: Int, hi0: Int, m: Int): Unit = {
    var lo = lo0
    var hi = hi0
    var stop = lo == hi
    while(!stop) {
      val pivot = partitionY(lo, hi, lo) // third arg lo is pivot but anyone would do

      if(pivot == m)
        stop = true
      else if(m < pivot)
        hi = pivot-1
      else
        lo = pivot+1
    }
  }

  protected def build(lo : Int, hi : Int, parent : Option[Internal]) : KdNode =
    if(hi-lo+1 <= bucketSize) {
      val bucket = Bucket(parent, lo, hi)
      for(i <- lo to hi)
        buckets(perms(i)) = bucket // all these points are stored in this bucket
      bucket
    }
    else {
      val m = (lo + hi) / 2

      val cutCoord = findMaxSpread(lo, hi)
      cutCoord match {
        case Coordinate.x => selectX(lo, hi, m)
        case Coordinate.y => selectY(lo, hi, m)
      }

      val cutVal = pointSet.coord(m, cutCoord)

      val internal = Internal(parent, cutCoord, cutVal)
      val someParent = Some(internal)
      internal.loSon = build(lo, m, someParent)
      internal.hiSon = build(m+1, hi, someParent)
      internal
    }

  def delete(i : Int): Unit = {
    val bucket = buckets(i)
    var j = bucket.lo
    while(perms(j) != i)
      j += 1
    swap(j, bucket.hi)
    bucket.hi -= 1

    if(bucket.lo > bucket.hi) {
      bucket.deleted = true
      var stop = false
      var internalOpt = bucket.parent
      while(!stop) {
        internalOpt match {
          case None =>
            stop = true
          case Some(internal) =>
            if(internal.loSon.deleted && internal.hiSon.deleted) {
              internal.deleted = true
              internalOpt = internal.parent
            } else
              stop = true
        }
      }
    }
  }

  private object Searcher {
    var nnTarget : Int = _
    var nnDist : Double = _
    var nnPtNum : Int = _

    def rnn(node : KdNode): Unit = {
      if(node.deleted)
        return

      node match {
        case bucket: Bucket =>
          for (i <- bucket.lo to bucket.hi) {
            val pt = perms(i)
            val dist = pointSet.distance(pt, nnTarget)
            if (dist < nnDist) {
              nnDist = dist
              nnPtNum = pt
            }
          }
        case internal: Internal =>
          val cutVal = internal.cutVal
          val targetVal = pointSet.coord(nnTarget, internal.cutCoord)
          if (targetVal < cutVal) {
            rnn(internal.loSon)
            if (targetVal + nnDist > cutVal)
              rnn(internal.hiSon)
          } else {
            rnn(internal.hiSon)
            if (targetVal - nnDist < cutVal)
              rnn(internal.loSon)
          }
      }
    }

    def topDown(i : Int): Int = {
      nnTarget = i
      nnDist = Double.MaxValue

      rnn(root)
      nnPtNum
    }
  }

  def nearestNeighbour(i : Int): Int =
    Searcher.topDown(i)
}


sealed trait KdNode {
  val parent : Option[Internal]
  var deleted : Boolean = false
}

case class Internal(parent : Option[Internal], cutCoord : Coordinate.Value, cutVal : Double) extends KdNode {
  var loSon : KdNode = null // these are really immutable but cyclic structure of tree prevents using vals
  var hiSon : KdNode = null
}

case class Bucket(parent : Option[Internal], lo : Int, var hi : Int) extends KdNode {
}


object Test extends App {
  val xs = Array( 4, 5,2,7, 8, 9,10,7,15,12,21,33,98, 45,23,0)
  val ys = Array(42,57,2,7,18,19,10,5,85,11,51,33,65,145,32,0)
  val pointSet = new point.ArrayPointSet(xs, ys)

  val kd = KdTree(pointSet)

  println(kd.nearestNeighbour(0))
  println(kd.nearestNeighbour(1))

  kd.delete(14)
  kd.delete(4)

  println(kd.nearestNeighbour(0))
  println(kd.nearestNeighbour(1))
}