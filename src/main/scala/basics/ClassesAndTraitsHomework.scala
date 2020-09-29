package basics

object ClassesAndTraitsHomework {

  class MutablePoint(var x: Double, var y: Double) {
    def move(dx: Double, dy: Double): Unit = {
      x = x + dx
      y = y + dy
    }

    override def toString: String =
      s"($x, $y)"
  }


  sealed trait Shape2D extends Located2D with Bounded2D with Movable2D {
    def area: Double
  }

  sealed trait Shape3D extends Located3D with Bounded3D with Movable3D {
    def surfaceArea: Double

    def volume: Double
  }

  // Traits for Shape2D and Shape3D to extend
  sealed trait Located2D {
    def x: Double

    def y: Double
  }

  sealed trait Located3D extends Located2D {
    def z: Double
  }

  sealed trait Bounded2D {
    def minX: Double

    def maxX: Double

    def minY: Double

    def maxY: Double
  }

  sealed trait Bounded3D extends Bounded2D {
    def minZ: Double

    def maxZ: Double
  }

  sealed trait Movable2D {
    def move(dx: Double, dy: Double): Shape2D
  }

  sealed trait Movable3D {
    def move(dx: Double, dy: Double, dz: Double): Shape3D
  }

  ///

  // 2D shapes
  final case class Point2D(x: Double, y: Double) extends Shape2D {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def move(dx: Double, dy: Double): Point2D = Point2D(x + dx, y + dy)

    override def area = 0

  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape2D {
    override def x: Double = centerX

    override def y: Double = centerY

    override def minX: Double = centerX - radius

    override def maxX: Double = centerX + radius

    override def minY: Double = centerY - radius

    override def maxY: Double = centerY + radius

    override def move(dx: Double, dy: Double): Circle = Circle(centerX + dx, centerY + dy, radius)

    override def area: Double = Math.PI * Math.pow(radius, 2)
  }

  final case class Rectangle(minX: Double, minY: Double, width: Double, height: Double) extends Shape2D {
    override def x: Double = minX + width / 2

    override def y: Double = minY + height / 2

    override def maxX: Double = minX + width

    override def maxY: Double = minY + height

    override def move(dx: Double, dy: Double): Rectangle = Rectangle(minX + dx, minY + dy, width, height)

    override def area: Double = width * height
  }

  final case class Square(minX: Double, minY: Double, side: Double) extends Shape2D {
    override def x :Double = minX + side / 2

    override def y: Double = minY + side / 2

    override def maxX: Double = minX + side

    override def maxY: Double = minY + side

    override def move(dx: Double, dy: Double): Square = Square(minX + dx, minY + dy, side)

    override def area: Double  = side * side
  }

   final case class Triangle(minX: Double , minY: Double , a:Double , b: Double, c: Double) extends Shape2D {
     val p: Double =(a+b+c)/2 // half of the perimeter
     override def area: Double = Math.sqrt(p*(p-a)*(p-b)*(p-c)) //Heron's formula
     val height: Double = 2*(area/a)
     override def maxX: Double = Math.max(minX +a , minX +Math.sqrt(c*c - height*height))
     override def maxY: Double = minY + height
     //centroid coordinates (Ox = (Ax + Bx + Cx)/3) , (Oy = (Ay + By + Cy)/3)
     // in this case we can use these formulas assuming that the triangle base (a) is parallel to coordinate system as it will be in this class
     override def x: Double = (minX + minX +a + minX +Math.sqrt(c*c - height*height)) /3
     override def y: Double = (minY + minY + maxY)/3
     override def move(dx: Double, dy: Double): Triangle = Triangle(minX + dx, minY + dy , a,b,c)
    }

  object Origin2D extends Located2D {
    override def x: Double = 0

    override def y: Double = 0
  }



  final case class Point3D(x: Double, y: Double , z: Double) extends Shape3D {
    override def minX: Double = x

    override def maxX: Double = x

    override def minY: Double = y

    override def maxY: Double = y

    override def minZ: Double = z

    override def maxZ: Double = z

    override def move(dx: Double, dy: Double , dz: Double): Point3D = Point3D(x + dx, y + dy , z+dz)

    override def surfaceArea: Double = 0

    override def volume: Double = 0
  }

  final case class Cube(square: Square, minZ: Double) extends Shape3D {

    override def minX: Double = square.minX

    override def minY: Double = square.minY

    val side: Double = square.side

    override def maxX: Double = minX + side

    override def maxY: Double = minY + side

    override def maxZ: Double = minZ + side

    override def x: Double = minX + side / 2

    override def y: Double = minY + side / 2

    override def z: Double  = minZ + side / 2

    override def surfaceArea: Double = square.area * 6

    override def volume: Double = square.area * side

    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(Square(minX + dx , minY +dy ,side), minZ + dz)

  }

  final case class Cuboid(minX: Double, minY: Double, minZ: Double , a: Double , b:Double, c: Double) extends Shape3D {
    override def maxX: Double = minX + a
    override def maxY: Double = minY + b
    override def maxZ: Double = minZ + c
    override def x: Double = maxX - a/2
    override def y: Double = maxY - b/2
    override def z: Double = maxZ - c/2
    override def surfaceArea: Double = 2*(a*b + a*c + b*c)
    override def volume: Double = a*b*c
    override def move(dx:Double, dy: Double, dz: Double):Cuboid = Cuboid(minX + dx, minY + dy , minZ + dz, a,b,c)
  }

  // I passed a 2D figure - square to Cube class , but I think it is more convenient to pass arguments explicitly to
  //3D Shape classes, because we still have to pass values to 2D Shape class, so in the following classes I won't pass circles
  final case class Sphere(x: Double , y: Double, z: Double, radius: Double) extends Shape3D {
    override def minX: Double = x - radius
    override def maxX: Double = x + radius
    override def minY: Double = y - radius
    override def maxY: Double = y + radius
    override def minZ: Double = z - radius
    override def maxZ: Double = z + radius
    override def surfaceArea: Double = 4 * Math.PI * radius * radius
    override def volume: Double  = (4 * Math.PI * Math.pow(radius , 3))/3
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(x+dx , y + dy , z + dz , radius)
  }

  final case class Cylinder(baseX:Double , baseY: Double , baseZ: Double, radius: Double , height: Double) extends Shape3D {
    override def x: Double = baseX
    override def y: Double = baseY
    override def z: Double = baseZ + height/2
    override def minX: Double  = baseX - radius
    override def maxX: Double = baseX + radius
    override def minY: Double  = baseY - radius
    override def maxY: Double = baseY + radius
    override def minZ: Double = baseZ
    override def maxZ: Double = baseZ + height
    override def surfaceArea: Double = 2 * Math.PI * radius * radius + 2* Math.PI * radius * height
    override def volume: Double = Math.PI * radius * radius * height
    override def move(dx: Double, dy: Double, dz: Double) :Cylinder = Cylinder(baseX + dx , baseY + dy, baseZ + dz , radius, height)
  }


  object Origin3D extends Located3D {
    override def x: Double = 0

    override def y: Double = 0

    override def z: Double = 0
  }



}
