import kdtree.KdTree
import point.{CachedArrayPointSet, PointSet}


object SimpleKdTreeTest extends App {
  // We firstly build a set with different points in problem
  val xs = Array[Int](5, 3, 12, 10,  6, 5, 1)
  val ys = Array[Int](8, 2,  9, 12, 11, 0, 5)
  val pointSet = CachedArrayPointSet(xs, ys)

  // We now build the corresponding k-d tree
  val kdTree = KdTree(pointSet)

  // We are going to query using point 0 as source for comparisons
  val src = 0
  kdTree.delete(src) // We remove source from k-d tree so that nearest one is not itself
  val (i, iDist) = kdTree.nearestNeighbor(src)

  println(s"The nearest neighbor to ${pointSet(src)} is ${pointSet(i)}")
  println(s"Their distance is $iDist")

  // We now remove the nearest point to get second nearest one
  kdTree.delete(i)
  val (j, jDist) = kdTree.nearestNeighbor(src)

  println(s"The second nearest neighbor to ${pointSet(src)} is ${pointSet(j)}")
  println(s"Their distance is $jDist")

  kdTree.undeleteAll() // We undelete all deleted points to restore initial state of the k-d tree

  // Lets now find all points within a radius of 5 units from source
  val radius = 5
  println(s"Searching for all points within $radius units from ${pointSet(src)}:")
  kdTree.delete(src) // We remove source from k-d tree so exclude it from search

  kdTree.withinRadius(src, radius,
    // this procedure is going to be called with each point satisfying the condition
    // and the corresponding distance
    (i, iDist) => {
      println(s"${pointSet(i)} is no further than $radius units from ${pointSet(src)}")
      println(s"Their distance is $iDist")
    }
  )
}



object TestKdTree extends App {
  def test(pointSet: PointSet): Unit = {
    val kd = KdTree(pointSet, 5)

    for (target <- 0 until pointSet.size) {
      val radius = 100
      kd.withinRadius(target, radius,
        (pt,d) => {
          println(pt, d)
          assert(d==pointSet.distance(pt, target) && d <= radius, "withinRadius test failed")
        })

      val sortedByDist = Array.range(0, pointSet.size).sortBy(pointSet.distance(target, _))

      println(sortedByDist.mkString(" "))
      // println(ps.map(i => (i,pointSet.distance(0,i))).mkString(" "))

      val kdSorted = new Array[Int](pointSet.size)
      for (i <- 0 until pointSet.size) {
        val (nn, _) = kd.nearestNeighbor(target)
        kdSorted(i) = nn
        kd.delete(nn)
      }
      println(kdSorted.mkString(" "))
      println()

      val ok = sortedByDist.sameElements(kdSorted) ||
        sortedByDist.map(pointSet.distance(target,_)).sameElements(kdSorted.map(pointSet.distance(target,_)))
      assert(ok, "test failed")

      kd.undeleteAll()
    }
  }

  for(seed <- 0 until 10) {
    val rnd = new scala.util.Random(seed)
    val ps = CachedArrayPointSet.uniform(rnd, 500, -1000, 1000)

    test(ps)
  }
}
