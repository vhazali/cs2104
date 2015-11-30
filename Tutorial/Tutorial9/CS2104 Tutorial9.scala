/*
  Tutorial 9 : 26th October 2015 (Scala) 

Compilation & Execution Instruction
===================================
  To compile this file, use either
         (i) scalac tut7.scala
     or (ii) fsc tut7.scala
  To run this class file, please use use:
     scala Tut7

  Note that fsc is a fast scala compiler which runs a
  compilation daemon in the background, to avoid the overheads
  of re-starting the compiler.

*/
abstract class Token
case class KeyW(s:String) extends Token {
  override def toString = "Key "+s
}
case class Id(s:String) extends Token {
  override def toString = "Id "+s
}
case class NumI(s:Int) extends Token {
  override def toString = "NumI "+s
}

object Keyword {
  val symbols = List("=","+","-","*","/","~","(",")")
  val alphs = List("let","in")
}
case class parseErr(smth:String)  extends Exception
object ParserC {
  type Parser[T,A] = List[T] => (A,List[T])
  def seq[T,A,B](p1:Parser[T,A],p2:Parser[T,B]) 
              (toks:List[T]) : ((A,B),List[T]) = {
    val (t1,tok1) = p1(toks)
    val (t2,tok2) = p2(tok1)
    ((t1,t2),tok2)
  }
  def alt[T,A,B](p1:Parser[T,A],p2:Parser[T,A]) 
              (toks:List[T]) : (A,List[T]) = {
    try {
      p1(toks)
    } catch {
      case parseErr(s) => p2(toks)
    }
  }
  def map[T,A,B](p:Parser[T,A],fn:A=>B) (toks:List[T])
  : (B,List[T])
  = {
        val (t1,tok1) = p(toks)
        (fn(t1),tok1)
  }
  def empty[T,A](toks:List[T]) :(List[A],List[T]) = {
    (Nil:List[A],toks)
  }
  def rep[T,A](p:Parser[T,A]) 
              (toks:List[T]) : (List[A],List[T]) = {
    def foo (t:(A,List[A])) = { t match {
                                case (a,b) => a::b
                              }}
    alt(map(seq(p,rep(p)),foo)
        ,empty[T,A]) (toks)
  }
  def explode (s:String) : List[Char] = {
    (s :\ (Nil:List[Char])) {(c,acc) => c::acc}  
  }

  def implode (lst:List[Char]) : String = {
    (lst :\ ("")) {(c,s) => c+s}  
  }
  type Tokens = List[Token]
  def symbol (s:String) (toks:Tokens) : (Unit,Tokens) = {
    def exc = new parseErr("Symbol "+s+" expected")
    toks match {
      case KeyW(b)::toks => if (s==b) ((),toks) else throw exc 
      case _ => throw exc 
    }
  }
}

object Tut9 extends App {
  // implicit def oTermtoString (x: OTerm) : String 
  // = x.toString
  println("Hello World")
  val good1a = "abaaaa"
  val good1b = "acaa"
  val bad1a = "acaaa"
  val bad1b = "a"
  val good2a = "addbc"
  val good2b = "ad"
  val bad2a = "addbcb"
  val bad2b = "ab"
  def rec_S1 (t:String) : Boolean = {
    val len = t.length
    def aux (i:Int) = {
      if (i>=len) false
      else if (t.charAt(i)=='a') aux1(i+1)
      else false
    }
    def aux1 (i:Int) : Boolean = {
      if (i>=len) false
      else if ((t.charAt(i)=='b') || (t.charAt(i)=='c')  
          || (t.charAt(i)=='d')) aux1(i+1)
      else aux2(i)
    }
    def aux2 (i:Int) : Boolean = {
      if (i+1>=len) false
      else if ((t.charAt(i)=='a') && (t.charAt(i+1)=='a')) {
        if (i+2==len) true
        else aux2(i+2)
      }
      else false
    }
    aux(0)
  }
  def rec_S2 (t:String) : Boolean =
    false

  def cfg_S (toks: List[Char]): (Boolean, List[Char]) = {
    map(seq(sym('('), seq(cfg_A,sym(')'))), {_ => true}) tok
  }

  def cfg_A (toks: List[Char]) : (Boolean, List[Char]) = {
    alt(
      alt( map(letter,{_ => true}),
        map( seq(sym'[',seq(cfg_A, sym']')),{_=>true} )),
      map(seq(seq(sym'{'), seq(cfg_A,sym('}'))),{_=>true})
        )
  }

  def test_rec (t:String) (rec_fn:String => Boolean) : Unit =
  {
      val ans = rec_fn(t);
      if (ans) println(t + " OK")
      else println(t + " Bad")
  }

  def sym (s:Char) (toks:List[Char]) : (Unit,List[Char]) = {
    def exc = new parseErr("Char "+s+" expected")
    toks match {
      case c::toks => if (s==c) ((),toks) else throw exc 
      case _ => throw exc 
    }
  }

  def letter (toks:List[Char]) : (Char,List[Char]) = {
    def exc = new parseErr("Char a-z expected")
    toks match {
      case c::toks => if ('a'<=c && c<='z') (c,toks) 
                      else throw exc 
      case _ => throw exc 
    }
  }
 import ParserC._
 def rec_S(toks:List[Char]) : (Boolean,List[Char]) = {
   def foo[A](t:A):Boolean = true
   map (seq(seq(sym('('),rec_S),sym(')')),foo) (toks)
 }

  val v1 = "hello"
  val v2 = explode(v1)
  println("v2: "+v2)
  println("imploded v2: "+implode(v2))
  println("\nTesting rec_S1\n")
  test_rec("abaaaa")(rec_S1)
  test_rec("ab")(rec_S1)
  test_rec("aba")(rec_S1)
  test_rec("abaa")(rec_S1)
  test_rec("ad")(rec_S1)
  test_rec("abcdd")(rec_S1)
  test_rec("abd")(rec_S1)
  test_rec(good1a)(rec_S1)
  test_rec(good1b)(rec_S1)
  test_rec(bad1a)(rec_S1)
  test_rec(bad1b)(rec_S1)
  println("\nTesting rec_S2\n")
  test_rec(good2a)(rec_S2)
  test_rec(good2b)(rec_S2)
  test_rec(bad2a)(rec_S2)
  test_rec(bad2b)(rec_S2)

  def rgx_S1 (t:String) : Boolean = {
    val rgx = """a(b|c|d)*(aa)+""".r
    t match {
      // case rgx(l1,l2,_*) =>
      //   println("l1: "+l1)
      //   println("l2: "+l2)
      //   true
      case rgx(_*) => true
      case _ => false
    }
  }
  def rgx_S2 (t:String) : Boolean = {
    val rgx = """a((bc)?d)+""".r
    t match {
      case rgx(_*) => true
      case _ => false
    }
  }
  println("\nTesting RGX S1\n")
  test_rec("ab")(rgx_S1)
  test_rec("aba")(rgx_S1)
  test_rec("abaa")(rgx_S1)
  test_rec("abcdaa")(rgx_S1)
  test_rec(good1a)(rgx_S1)
  test_rec(good1b)(rgx_S1)
  test_rec(bad1a)(rgx_S1)
  test_rec(bad1b)(rgx_S1)
  println("\nTesting RGX S2\n")
  test_rec("abcdd")(rgx_S2)
  test_rec("abcdbcd")(rgx_S2)
  test_rec("abcdbd")(rgx_S2)
  test_rec("a")(rgx_S2)
  test_rec("abaaaa")(rgx_S2)
  test_rec(good2a)(rgx_S2)
  test_rec(good2b)(rgx_S2)
  test_rec(bad2a)(rgx_S2)
  test_rec(bad2b)(rgx_S2)

}
