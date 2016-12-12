

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
    var tr = 0
    var tg = 0
    var tb = 0
    var ta = 0

    val i_min = clamp(x - radius, 0, src.width - 1)
    val j_min = clamp(y - radius, 0, src.height - 1)

    val ii = clamp(x + radius, 0, src.width - 1)
    val jj = clamp(y + radius, 0, src.height - 1)

    var k = 0

    var i = i_min

    while (i <= ii) {
      var j = j_min

      while (j <= jj) {
        val cell = src(i, j)
        tr += red(cell)
        tg += green(cell)
        tb += blue(cell)
        ta += alpha(cell)
        k += 1
        j += 1
      }
      i += 1
    }
    val rr = tr / k
    val rg = tg / k
    val rb = tb / k
    val ra = ta / k
    rgba(rr, rg, rb, ra)
  }

}
