import Element.elem

abstract class Element {
  def contents: Array[String]
  def heigth: Int = contents.length
  def width: Int = if(contents.isEmpty) 0 else contents(0).length

  def above (that: Element): Element ={
    val this1 = this widen that.width
    val that1 = that widen this.width

    elem(this1.contents ++ that1.contents)
  }

  def beside (that: Element): Element = {
    val this1 = this heigthen that.heigth
    val that1 = that heigthen this.heigth
    elem(
      for(
        (line1, line2) <- this1.contents zip that1.contents
      ) yield line1 + line2
    )

  }

  def widen(w: Int): Element = {
    if(w <= width) this
    else {
      val left = elem(' ', (w - width) / 2, heigth)
      val right = elem(' ', (w - width - left.width), heigth)
      left beside this beside right
    }
  }

  def heigthen(h: Int): Element = {
    if (h <= heigth) this
    else {
      val top = elem(' ', width, (h - heigth) / 2)
      val bot = elem(' ', width, h - heigth - top.heigth)
      top above this above bot
    }
  }

  override def toString: String = contents mkString "\n"

}


object Element{

  def elem(contents: Array[String]): Element =
    new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element =
    new UniformElement(chr, width, height)

  def elem(line: String): Element =
    new LineElement(line)

  private class UniformElement(
    ch: Char,
    override val width: Int,
    override val heigth: Int
  ) extends Element {
    private val line = ch.toString * width
    def contents = Array.fill(heigth)(line)
  }

  private class ArrayElement(
    val contents: Array[String])
  extends Element

  private class LineElement(s: String) extends Element{
    val contents = Array(s)
    override def heigth = s.length
    override def width: Int = 1
  }

}

