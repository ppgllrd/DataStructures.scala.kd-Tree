import heap.BinaryHeap

object Test extends App {
  val h = new BinaryHeap[Int](100)
  h.insert(10, 110)
  h.insert(20, 120)
  h.insert(5, 105)
  h.insert(15, 115)


  for (_ <- 0 until 4) {
    println(h.minElem())
    h.delMin()
  }

  println()


  def test(seed : Int): Unit = {
    val h2 = new BinaryHeap[Int](100)
    val rnd = new scala.util.Random(seed)
    val numElems = 50

    val xs = new Array[(Double,Int)](numElems)

    for (i <- 0 until numElems) {
      val d = rnd.nextInt(500)
      val e = d+1000
      h.insert(d,e)
      xs(i) = (d,e)
    }


    val sorted = xs.sortBy(_._1)

    for (i <- 0 until numElems) {
      val m@(d,e) = h.minElem()
      println(m)
      h.delMin()

      if(m != sorted(i))
        sys.error("Test failed")
    }
  }

  for(s <- 0 until 100)
    test(s)
}