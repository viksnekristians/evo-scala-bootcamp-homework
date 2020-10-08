package basics

object BasicsHomework {

  def gcd (a: Int, b:Int) : Int = {
    if (b != 0) Math.abs(gcd(b, a % b))
    else a // if both numbers are 0, return value = 0
  }
  def lcm(a:Int , b:Int): Int =  {
    if(a==0 || b==0)  0 else Math.abs(a*b/gcd(a,b))
  }

  def encryptThis(text: String): String = {
    text.split(" ").map( str =>
      str.length match {
        case 0 => ""
        case 1 => s"${str(0).toInt}"
        case 2 => s"${str(0).toInt}" + str(str.length-1)
        case _ => s"${str(0).toInt}" + str(str.length-1)  + str.slice(2, str.length-1) + str(1)
      }

    ).mkString(" ")
  }

}
