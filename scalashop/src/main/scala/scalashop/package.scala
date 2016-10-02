
import common._

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    var sx = x - radius
    var sy = y - radius
    var no = 0
    var sumRed = 0
    var sumBlue = 0
    var sumGreen = 0
    var sumAlpha = 0
    while (sx < src.width && sx <= x + radius) {
      var sy = y - radius
      while (sy < src.height && sy <= y + radius) {
        sy = clamp(sy, 0, src.height)
        sx = clamp(sx, 0, src.width)
        sumRed += red(src.apply(sx, sy))
        sumGreen += green(src.apply(sx, sy))
        sumBlue += blue(src.apply(sx, sy))
        sumAlpha += alpha(src.apply(sx, sy))
        no = no + 1;
        sy = sy + 1
      }
      sx = sx + 1
    }
    if (no == 0) {
      src.apply(x, y)
    } else {
      rgba(sumRed / no, sumGreen / no, sumBlue / no, sumAlpha / no)
    }
  }
}
