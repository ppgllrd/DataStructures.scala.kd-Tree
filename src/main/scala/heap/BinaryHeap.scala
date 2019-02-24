/** ****************************************************************************
  * Binary min heap
  *
  * Pepe Gallardo, 2019
  *
  * ****************************************************************************/

package heap

object BinaryHeap {
  private val root = 1

  def apply[A: Manifest](capacity: Int): BinaryHeap[A] =
    new BinaryHeap[A](capacity)
}

class BinaryHeap[@specialized A: Manifest](capacity: Int) {
  protected val orders = new Array[Double](capacity + 1)
  protected val elements = new Array[A](capacity + 1)
  protected var nextFree: Int = 1

  def size: Int = nextFree - 1

  def isEmpty: Boolean =
    nextFree <= BinaryHeap.root

  def minElem(): (Double, A) = {
    // assert(!isEmpty, "BinaryHeap.top: empty heap")
    (orders(BinaryHeap.root), elements(BinaryHeap.root))
  }

  private def heapifyUp(idx: Int, order: Double, element: A): Unit = {
    var node = idx
    var stop = false
    while (!stop && node > BinaryHeap.root) {
      val parent = node / 2
      val orderParent = orders(parent)
      if (orderParent > order) {
        orders(node) = orderParent
        elements(node) = elements(parent)

        node = parent
      } else
        stop = true // heap order property holds
    }

    orders(node) = order
    elements(node) = element
  }

  def insert(order: Double, element: A): Unit = {
    // assert(nextFree<distances.length, "BinaryHeap.insert: Out of memory")
    heapifyUp(nextFree, order, element)
    nextFree += 1
  }

 private def heapifyDown(from : Int, order: Double, element: A): Unit = {
    var node = from
    var minChild = 2 * node

    var stop = false
    while (!stop && minChild < nextFree) {
      var minOrder = orders(minChild)

      val rightChild = minChild + 1
      if (rightChild < nextFree) {
        // has right child
        val orderRightChild = orders(rightChild)
        if (orderRightChild < minOrder) {
          // and right child stores minimum value
          minOrder = orderRightChild
          minChild = rightChild
        }
      }

      if (minOrder < order) {
        orders(node) = orders(minChild)
        elements(node) = elements(minChild)

        // continue from minChild on next iteration
        node = minChild
        minChild = 2 * node
      }
      else
        stop = true // heap order property holds
    }
    orders(node) = order
    elements(node) = element
  }


  def delMin(): Unit = {
    //assert(!isEmpty, "BinaryHeap.delMin: empty heap")
    nextFree -= 1
    // move last element to root and restore heap order property
    heapifyDown(BinaryHeap.root, orders(nextFree), elements(nextFree))
  }

  def replaceMin(order: Double, element: A): Unit = {
    // restore heap order property
    heapifyDown(BinaryHeap.root, order, element)
  }
}


