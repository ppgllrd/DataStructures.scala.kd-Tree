/** ****************************************************************************
  * Semidynamic K-d Trees
  *
  * Pepe Gallardo, 2019
  *
  * See:
  *   K-d trees for semidynamic point sets
  *   Jon Louis Bentley
  *   SCG '90 Proceedings of the sixth annual symposium
  *   on Computational Geometry, 1990
  *
  * ****************************************************************************/

package kdtree

import point.{Axis, PointSet}


case class KdTree(pointSet: PointSet, bucketSize: Int = 5) {
  private var dynamicSize: Int = pointSet.size

  def size: Int = dynamicSize

  // a permutation of points in pointSet
  protected val perms = new Array[Int](pointSet.size)
  for (i <- perms.indices)
    perms(i) = i // start with identity permutation

  // buckets(i) is bucket where pointSet(i) is stored
  protected val buckets = new Array[Bucket](pointSet.size)

  protected val root: KdNode = {
    val (xMin, yMin, xMax, yMax) = pointSet.bounds
    build(0, pointSet.size - 1, None, xMin, yMin, xMax, yMax)
  }

  private def swap(i: Int, j: Int): Unit = {
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
  private def partitionX(lo: Int, hi: Int, pivot: Int): Int = {
    val pivotVal = pointSet.x(perms(pivot))

    // swap elements at hi and pivot
    swap(hi, pivot)

    var p = lo
    for (i <- lo until hi) {
      if (pointSet.x(perms(i)) < pivotVal) {
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
    while (!stop) {
      val pivot = partitionX(lo, hi, m) // third arg m is pivot but anyone would do

      if (pivot == m)
        stop = true
      else if (m < pivot)
        hi = pivot - 1
      else
        lo = pivot + 1
    }
  }

  private def partitionY(lo: Int, hi: Int, pivot: Int): Int = {
    val pivotVal = pointSet.y(perms(pivot))

    // swap elements at hi and pivot
    swap(hi, pivot)

    var p = lo
    for (i <- lo until hi) {
      if (pointSet.y(perms(i)) < pivotVal) {
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
    while (!stop) {
      val pivot = partitionY(lo, hi, m) // third arg m is pivot but anyone would do

      if (pivot == m)
        stop = true
      else if (m < pivot)
        hi = pivot - 1
      else
        lo = pivot + 1
    }
  }

  private def build(lo: Int, hi: Int, parentOpt: Option[Internal], xMin:Int, yMin:Int, xMax:Int, yMax:Int): KdNode =
    if (hi - lo + 1 <= bucketSize) {
      val bucket = Bucket(parentOpt, lo, hi, hi, xMin, yMin, xMax, yMax)
      for (i <- lo to hi)
        buckets(perms(i)) = bucket // all these points are stored in this bucket

      bucket
    }
    else {
      val m = (lo + hi) / 2

      val (cutAxis, cutVal) =
        if(xMax - xMin > yMax - yMin) { // select cut axis according to max spread
          selectX(lo, hi, m)
          (Axis.x, pointSet.x(perms(m)))
        } else {
          selectY(lo, hi, m)
          (Axis.y, pointSet.y(perms(m)))
        }

      val internal = Internal(parentOpt, cutAxis, cutVal, xMin, yMin, xMax, yMax)
      val someParent = Some(internal)

      cutAxis match {
        case Axis.x =>
          internal.loSon = build(lo, m, someParent, xMin, yMin, cutVal, yMax)
          internal.hiSon = build(m + 1, hi, someParent, cutVal, yMin, xMax, yMax)
        case Axis.y =>
          internal.loSon = build(lo, m, someParent, xMin, yMin, xMax, cutVal)
          internal.hiSon = build(m + 1, hi, someParent, xMin, cutVal, xMax, yMax)
      }

      internal
    }

  def delete(i: Int): Unit = {
    val bucket = buckets(i)
    var j = bucket.lo
    while (perms(j) != i)
      j += 1
    swap(j, bucket.hi)
    bucket.hi -= 1

    dynamicSize -= 1

    if (bucket.lo > bucket.hi) {
      bucket.deleted = true

      var stop = false
      var internalOpt = bucket.parent
      while (!stop) {
        internalOpt match {
          case None =>
            stop = true
          case Some(internal) =>
            if (internal.loSon.deleted && internal.hiSon.deleted) {
              internal.deleted = true
              internalOpt = internal.parent
            } else
              stop = true
        }
      }
    }
  }

  def undelete(i: Int): Unit = {
    val bucket = buckets(i)
    var j = bucket.end
    while (perms(j) != i)
      j -= 1
    bucket.hi += 1
    swap(j, bucket.hi)

    dynamicSize += 1

    bucket.deleted = false

    var stop = false
    var internalOpt = bucket.parent
    while (!stop) {
      internalOpt match {
        case None =>
          stop = true
        case Some(internal) =>
          internal.deleted = false
          internalOpt = internal.parent
      }
    }
  }

  def undeleteAll(): Unit = {
    def aux(node: KdNode): Unit = {
      node.deleted = false
      node match {
        case b: Bucket =>
          b.hi = b.end
        case internal: Internal =>
          aux(internal.loSon)
          aux(internal.hiSon)
      }
    }

    dynamicSize = pointSet.size
    aux(root)
  }

  private def circleInBounds(i: Int, radius: Double, xMin: Int, yMin: Int, xMax:Int, yMax: Int) : Boolean = {
    val x = pointSet.x(i)
    val y = pointSet.y(i)
    (x-radius) >= xMin && (x+radius) <= xMax &&
      (y-radius) >= yMin && (y+radius) <= yMax
  }

  private object Searcher {
    var target: Int = _
    var minDist: Double = _
    var nn: Int = _

    def rnn(node: KdNode): Unit = {
      if (node.deleted)
        return

      node match {
        case bucket: Bucket =>
          for (i <- bucket.lo to bucket.hi) {
            val pt = perms(i)
            val dist = pointSet.distance(pt, target)
            if (dist < minDist) {
              minDist = dist
              nn = pt
            }
          }
        case internal: Internal =>
          val cutVal = internal.cutVal
          val targetVal = pointSet.coord(target, internal.cutAxis)
          if (targetVal < cutVal) {
            rnn(internal.loSon)
            if (targetVal + minDist > cutVal)
              rnn(internal.hiSon)
          } else {
            rnn(internal.hiSon)
            if (targetVal - minDist < cutVal)
              rnn(internal.loSon)
          }
      }
    }

    def topDown(i: Int): Int = {
      target = i
      minDist = Double.MaxValue

      rnn(root)
      nn
    }

    def bottomUp(i: Int): Int = {
      target = i
      minDist = Double.MaxValue

      var node : KdNode = buckets(target)
      rnn(node)

      var stop = false
      while(!stop) {
        node.parent match {
          case None =>
            stop = true
          case Some(internal) =>
            val lastNode = node
            node = internal
            val diff = pointSet.coord(target, internal.cutAxis) - internal.cutVal
            if(minDist >= diff) {
              if(lastNode == internal.loSon)
                rnn(internal.hiSon)
              else
                rnn(internal.loSon)
            }
            if(circleInBounds(target, minDist, internal.xMin, internal.yMin, internal.xMax, internal.yMax))
              stop = true
        }
      }

      nn
    }
  }

  def nearestNeighbour(i: Int): Int =
    Searcher.bottomUp(i)
}


sealed trait KdNode {
  val parent: Option[Internal]
  var deleted: Boolean = false

  val xMin, yMin, xMax, yMax: Int
}

case class Internal(parent: Option[Internal], cutAxis: Axis.Value, cutVal: Double, xMin:Int, yMin:Int, xMax:Int, yMax:Int) extends KdNode {
  var loSon: KdNode = null // these are really immutable but cyclic structure of tree prevents using vals
  var hiSon: KdNode = null
}

case class Bucket(parent: Option[Internal], lo: Int, var hi: Int, end: Int, xMin:Int, yMin:Int, xMax:Int, yMax:Int) extends KdNode

