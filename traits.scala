class Point(val x: Int, val y: Int)

class Rectangle(val topLeft: Point, val bottomRight: Point) extends Rectangular {
  // other methods...
}

abstract class Component extends Rectangular {
  // other methods...
}

trait Rectangular {
  def topLeft: Point
  def bottomRight: Point

  def left = topLeft.x
  def right = bottomRight.x
  def width = right - left
  // and more...
}