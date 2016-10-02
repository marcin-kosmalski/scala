package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /**
   * Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (x <- from until end) {
      for (y <- 0 until src.height) {
        dst.update(x, y, boxBlurKernel(src, x, y, radius))
      }
    }
  }

  /**
   * Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val stripsNo = src.width / numTasks
    if (stripsNo == 0) {
      for (x <- 0 until src.width) {
        blur(src, dst, x, x + 1, 1)
      }
    } else {
      val taskList=for {
        p <- distribute(src.width, numTasks)
      } yield (task{blur(src, dst, p._1, p._2, radius)})
      
      taskList.foreach { task => task.join() }
      
    }

  }

  def distribute(size: Int, tasks: Int): IndexedSeq[(Int, Int)] = {

    val div = size / tasks
    val mod = size % tasks

    def f = (x: Int) => if (x < mod) 1 else 0
    val list = for {
      i <- 0 until tasks
    } yield (div + f(i))
    def sum = (i: Int) => if (i < 0) 0 else list.slice(0, i + 1).foldLeft(0)(_ + _)
    for {
      i <- 0 until tasks
    } yield ((sum(i - 1), sum(i)))
  }

}
