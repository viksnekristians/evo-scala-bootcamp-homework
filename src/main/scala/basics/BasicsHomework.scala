package basics

object BasicsHomework {

  def gcd (a: Int, b:Int) : Int = {
    if (b != 0) Math.abs(gcd(b, a % b))
    else a // if both numbers are 0, return value = 0
  }
  def lcm(a:Int , b:Int): Int =  {
    if(a==0 || b==0)  0 else Math.abs(a*b/gcd(a,b))
  }

}
