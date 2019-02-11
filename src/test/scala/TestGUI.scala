import gui.Window2D
import point.{CachedArrayPointSet, PointSet}

import scala.swing.{Graphics2D, SimpleSwingApplication}

class TestGUI(pointSet: PointSet) extends SimpleSwingApplication {
  def top = new Window2D() {
    override def paintProcedure(g2D: Graphics2D, component: java.awt.Component): Unit =
      pointSet.paint(g2D, component)
  }
}

object TestGUI1 extends {
  val rnd = new scala.util.Random(0)
  val pointSet = CachedArrayPointSet.uniform(rnd, 150, -1000, 1000)
  } with TestGUI(pointSet)


object TestGUI2 extends {
  val rnd = new scala.util.Random(0)
  val pointSet = CachedArrayPointSet.normal(rnd, 150, 1)
  } with TestGUI(pointSet)



