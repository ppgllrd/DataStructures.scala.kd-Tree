import kdtree.KdTree
import point.{CachedArrayPointSet, PointSet}

object TestKdTree extends App {
  def test(pointSet: PointSet): Unit = {
    val kd = KdTree(pointSet, 5)

    for (target <- 0 until pointSet.size) {
      val sortedByDist = Array.range(0, pointSet.size).sortBy(pointSet.distance(target, _))

      println(sortedByDist.mkString(" "))
      // println(ps.map(i => (i,pointSet.distance(0,i))).mkString(" "))

      val kdSorted = new Array[Int](pointSet.size)
      for (i <- 0 until pointSet.size) {
        val nn = kd.nearestNeighbour(target)
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
