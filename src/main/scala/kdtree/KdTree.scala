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

object KdTree {
  val bucketSize = 3
}

case class KdTree(pointSet: PointSet) {
  val size : Int = pointSet.size

  protected val perm = new Array[Int](size) // permutation of points
  for(i <- perm.indices)
    perm(i) = i // start with identity permutation

  protected val bucket = new Array[Bucket](size)

  protected val root : KdNode = build(0, size-1, None)

  private def findMaxSpread(lo : Int, hi : Int) : Coordinate.Value = {
    val plo = perm(lo)
    var minX = pointSet.x(plo)
    var maxX = pointSet.x(plo)
    var minY = pointSet.y(plo)
    var maxY = pointSet.y(plo)

    var i = lo+1
    while(i <= hi) {
      val pi = perm(i)
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
    val tmp = perm(i)
    perm(i) = perm(j)
    perm(j) = tmp
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
    val pivotValue = pointSet.x(perm(pivot))

    // swap elements at hi and pivot
    swap(hi, pivot)

    var p = lo
    for(i <- lo until hi) {
      if(pointSet.x(perm(i)) < pivotValue) {
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
    val pivotValue = pointSet.y(perm(pivot))

    // swap elements at hi and pivot
    swap(hi, pivot)

    var p = lo
    for(i <- lo until hi) {
      if(pointSet.y(perm(i)) < pivotValue) {
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
    if(hi-lo+1 <= KdTree.bucketSize) {
      val b = Bucket(parent, lo, hi)
      for(i <- lo to hi)
        bucket(perm(i)) = b // all these points are stored in this bucket
      b
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
      internal.loson = build(lo, m, someParent)
      internal.hison = build(m+1, hi, someParent)
      internal
    }

  def delete(i : Int): Unit = {
    val b = bucket(i)
    var j = b.lopt
    while(perm(j) != i)
      j += 1
    swap(j, b.hipt)
    b.hipt -= 1

    if(b.lopt > b.hipt) {
      b.deleted = true
      var stop = false
      var nodeOpt = b.parent
      while(!stop) {
        nodeOpt match {
          case None =>
            stop = true
          case Some(node) =>
            if(node.loson.deleted && node.hison.deleted) {
              node.deleted = true
              nodeOpt = node.parent
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

    def rnn(p : KdNode): Unit = {
      if(p.deleted)
        return

      p match {
        case p: Bucket =>
          for (i <- p.lopt to p.hipt) {
            val pt = perm(i)
            val dist = pointSet.distance(pt, nnTarget)
            if (dist < nnDist) {
              nnDist = dist
              nnPtNum = pt
            }
          }
        case p: Internal =>
          val cutVal = p.cutVal
          val targetCoord = pointSet.coord(nnTarget, p.cutCoord)
          if (targetCoord < cutVal) {
            rnn(p.loson)
            if (targetCoord + nnDist > cutVal)
              rnn(p.hison)
          } else {
            rnn(p.hison)
            if (targetCoord - nnDist < cutVal)
              rnn(p.loson)
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
  var loson : KdNode = null
  var hison : KdNode = null
}

case class Bucket(parent : Option[Internal], lopt : Int, var hipt : Int) extends KdNode {
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