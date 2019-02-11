package gui

import scala.swing.{Dimension, Graphics2D, Label, MainFrame}

class Window2D(t: String = "Window2D", width: Int = 400, height: Int = 400) extends MainFrame {
  import java.awt.RenderingHints

  title = t
  contents = label

  def paintProcedure(g2D: Graphics2D, component: java.awt.Component): Unit = {}

  private def label = new Label {
    preferredSize = new Dimension(width, height)

    override def paintComponent(g2D: Graphics2D) {
      super.paintComponent(g2D)
      g2D.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
      g2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      val w = peer.getWidth
      val h = peer.getHeight

      // Set cartesian axes. Scale is 1 unit = 1 pixel
      // val at = g2D.getTransform
      // g2D.translate(w/2, h/2)
      // g2D.scale(1, -1)
      paintProcedure(g2D, peer)
      // g2D.setTransform(at)
    }
  }
}

