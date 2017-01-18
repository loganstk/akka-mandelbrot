package mandelbrot

import akka.actor.{Actor, ActorSystem, Props}
import akka.routing.BalancingPool

import scalafx.application.{JFXApp, Platform}
import scalafx.scene.Scene
import scalafx.scene.image.{ImageView, PixelWriter, WritableImage}
import scalafx.scene.paint.Color

object MandelbrotActors extends JFXApp {
  val maxCount = 10000
  val imageSize = 600
  val XMin: Double = -1.5
  val XMax: Double = 0.5
  val YMin: Double = -1.0
  val YMax: Double = 1.0

  case class Complex(re: Double, im: Double) {
    def +(that: Complex): Complex = Complex(re + that.re, im + that.im)
    def *(that: Complex): Complex = Complex(
      re * that.re - im * that.im, re * that.im + im * that.re
    )
    def magnitude: Double = math.sqrt(re * re + im * im)
  }

  def mandelCount(c: Complex): Int = {
    var count = 0
    var z = Complex(0, 0)
    while (count < maxCount && z.magnitude < 2) {
      z = z*z + c
      count += 1
    }
    count
  }

  case class Line(row: Int, y: Double)

  class LineActor(pw: PixelWriter) extends Actor {
    override def receive: Receive = {
      case Line(row, y) =>
        for (j <- 0 until imageSize) {
          val x = XMin + j * (XMax - XMin) / imageSize
          val count = mandelCount(Complex(x, y))
          Platform.runLater {
            pw.setColor(j, row, if (count == maxCount) Color.Black else {
              val scale = 10 * math.sqrt(count.toDouble / maxCount) min 1.0
              Color(scale, 0, 0, 1)
            })
          }
        }
    }
  }

  val system = ActorSystem("MandelSystem")

  stage = new JFXApp.PrimaryStage {
    title = "Mandelbrot Actors"
    scene = new Scene(imageSize, imageSize) {
      val image = new WritableImage(imageSize, imageSize)
      content = new ImageView(image)
      val router = system.actorOf(BalancingPool(4).props(Props(new LineActor(image.pixelWriter))), "Pool")
      for (i <- 0 until imageSize) {
        val y = YMin + i * (YMax - YMin) / imageSize
        router ! Line(i, y)
      }
    }
  }
}
