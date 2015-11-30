object Lab4Add extends App {
	def conv (x:Option[String]): Int =
		x match {
			case None => -1
			case Some(v) => v.length
		}

	println(conv(None))
	println(conv(Some("Hello")))

	
	/*
	 * Using the higher order method, fold left,
	 * We simply supply the None value, in this case -1,
	 * Then we supply the value to be given for some, in this case,
	 * we supply the length of the element
	*/

	def conv_ho (x:Option[String]): Int =
		x.foldLeft(-1)((b,a)=>a.length)

	println(conv_ho(None))
	println(conv_ho(Some("Hello")))

	def insert(x:Int, ys: List[Int]) : List[Int]
  		= ys match {
    case Nil => List(x)
    case y::ys => 
      if (x <= y) x::y::ys
      else y::insert(x,ys)
  	}

  	/*
  	 * println(insert(5,null))
  	 * This will fail, as null != Nil
  	 * Nil is essentially a special object used to represent empty list
  	 * Wherese null is an object, that is of any type, used to denote something
  	 * that has yet to exist
  	*/


  	/*
	 * Defining ordering for list of Integers
	 */
  	implicit object ListInt extends Ordering[List[Int]] {
  		def compare (x:List[Int],y:List[Int])
  			= x.length()-y.length()
  	}
}