/*

 Tut 8/Lab 4 : From OCaml to Scala Programming: 
 Submission by 28th Oct 2015 (for Q6-Q11)

 We will try the first few 5 questions during the tutorial,
 and you can complete the lab assignment yourself thereafter.
 Please provide a couple of test cases for each example.
 Please use qn_i for the solution of each of your method.
 For example, your final solution for Q8 should be named qn_8.

*/

object Lab4 extends App {
  println("Hello to Lab4/Tutorial8")
  /* 
  Q1 : Write a recursive function that would return the
       last element of the list. In case of empty list,
       throw a Failure exception.

       What is the polymorphic type of this function?
  */

  /* xs is the parameter of the last function.
   * X is the type parameter of the last function since last is a polymorphic function
   * It helps with the compiler to know the type of the result
  */
  def last [X](xs:List[X]) : X 
  = xs match {
    case Nil => throw new Exception("no last element")
    case x::xs => xs match {
      case Nil => x
      case _ => last(xs)
    }
  }

  def qn_1 [X] (xs:List[X]) = last (xs)

  // println(qn_1(List(1,2,3,4)))
  // println(qn_1(List('a','b','c','d')))
  // println(qn_1(List("a","bb","ccc")))
  // println(qn_1(List()))

  /* 
  Q2 : Change the last function to one with the following
       type: 'a list -> 'a option
       This function should return Some v, where v is the
       last element of the list. If the list is empty, you
       should return None.
   let last2 (xs: 'a list) : 'a option = 
   failwith "last2 not implemented yet"
  */

  def last2 [X](xs:List[X]): Option[X]
  = xs match {
    case Nil => None
    case x::xs => xs match {
      case Nil => Some(x)
      case _ => last2(xs)
    }
  }

  def qn_2[X](xs:List[X]) = last2(xs)
  // println(qn_2(List(1,2,3,4)))
  // println(qn_2(List('a','b','c','d')))
  // println(qn_2(List("a","bb","ccc")))
  // println(qn_2(List()))

/* 
  Q3 : Write a recursive function to sort a list of numbers
       using the insertion sort method.

       For your convenience, we have provided an
       insert procedure.
       (i) can you improve the insert method to
           avoid constructing (y::ys) in the base case?
           (Hint : use the as-pattern notation)
      (ii) implement a recursive sort method

 let rec insert x ys =
  match ys with
    | [] -> [x]
    | y::ys -> 
          if x<=y then x::y::ys
          else y::(insert x ys)
let sort xs=
  failwith "sort method based on insertion sort"

*/

/* def sorted [B >: A] (implicit ord: math.Ordering[B]): List[A]
 * Note that implicit here means that it can either be supplied or not
 * Note that B is more "general" than A, or A is a subtype of B
 */ 
def insert_int (x:Int, ys:List[Int]) : List[Int]
  = ys match {
    case Nil => List(x)
    case y::ys =>
      if (x<=y) x::y::ys
      else y::insert_int(x,ys)
  }
 // println(insert_int(4, List(1,2,3,5)))

def insert_gen [A] (x:A, ys: List[A])(implicit ord: math.Ordering[A]) : List[A]
  = ys match {
    case Nil => List(x)
    case y::ys => 
      if (ord.lteq(x,y)) x::y::ys   //lteq(x,y) == x less than or equals y
      else y::insert_gen(x,ys)
}

// println(insert_gen(4,List(1,2,3,5))(scala.math.Ordering.Int))
// So if the ordering wants to be redefined, we can replace the second part of the method
// println(insert_gen("bb",List("aa","ddd")))


  def sort[A] (xs:List[A])(implicit ord: math.Ordering[A]) :List[A] = {
    var res = List[A]()
    if (xs.length < 1) return res
    for (x <- xs) res = insert_gen(x, res)
    return res 
  }

  def printList(args: List[_]): Unit = {
    print("[")
    args.foreach(print)
    print ("]\n")
  }

  def qn_3 (xs:List[Int]): List[Int] = sort(xs)

  // printList(qn_3(List(4,2,5,1,3)))
  // printList(qn_3(List()))
  // printList(qn_3(List("a","bb","ccc")))
  // printList(qn_3(List('a','b','c')))

/* 
  Q4 : Consider a uprim type to capture either 
       integer, float or a string value.

       You can build a list of mixed type using
       it, and can perform List.rev and List.length
       using it.

       Compute the sum of mixed list using the value_of_mix
       function.
type uprim = I of int | F of float | S of string ;;

let mix_ls = [I 3; F 4.3; S "hello"; I 4];;

print_endline ("mix_ls has length "^(string_of_int (List.length mix_ls)));;
List.rev  mix_ls;;

let value_of_mix up =
  match up with
    | I v -> v
    | F v -> (int_of_float v) (* truncates the float value *)
    | S s -> (String.length s) (* length of string *)

*/

abstract class uprim {
  case class uprimInt(i: Int) extends uprim
  case class uprimFloat(f: Float) extends uprim
  case class uprimString(s: String) extends uprim
}

// def value_of_mix (x: uprim) : Int = x match{
//   case i: uprimInt => i
//   case f: uprimFloat => f.toInt
//   case s: uprimString => s.length
// }

val mix_ls = List (3,4.3,"hello",4)

def qn_4 = {
  println("Mixed list: "+mix_ls)
  println("Length of mixed list: "+mix_ls.length)
  println("Reversed list: "+ mix_ls.reverse)
}

qn_4

/* 
  Q5 : Consider a polymorphic tree.

       Write a function that will return the largest value in
       the tree. You may use the max function.
type 'aa btree = Leaf of 'aa | Node of 'aa * ('aa btree) * ('aa btree) ;;
let t1 = Leaf 3;;
let t2 = Node(4,t1,t1);;
let t2 = Node(6,t2,t1);;

let rec max_tree (t: int btree) : int =
  failwith "max_tree to be implemented"
*/

  import math.max

  abstract class Tree[X]
  case class Leaf[X] (x:X) extends Tree[X]
  case class Node[X] (x:X, lt:Tree[X], rt:Tree[X]) extends Tree[X]

  def max_tree_int(t: Tree[Int]) : Int
    = t match {
      case Leaf(v) => v
      case Node(v,lt,rt) => max(v,max(max_tree_int(lt),max_tree_int(rt)))
    }

  // println(max_tree_int(Node(2,Leaf(1),Leaf(4))))

  def max_tree_gen[A] (t: Tree[A])(implicit ord: math.Ordering[A]) : A
    = t match {
      case Leaf(v) => v
      case Node(v,lt,rt) => ord.max(v,ord.max(max_tree_gen(lt),max_tree_gen(rt)))
    }

  def qn_5[A] (t: Tree[A])(implicit ord: math.Ordering[A]) = max_tree_gen[A] (t)

  // println(qn_5(Node(4,Leaf(1),Leaf(6))))
  // println(qn_5(Leaf(1)))
  // println(qn_5(Node('a',Leaf('b'),Leaf('c'))))
  // println(qn_5(Leaf()))

/* 
  Q6 : Below is a function that will flatten a tree into a list
       by traversing the tree in an infix-order.

       Write another function that will flatten a tree in
       based on pre-fix traversal.
let rec flatten_infix (t: 'a btree) : 'a list =
  match t with
    | Leaf v -> [v]
    | Node(v,lt,rt) -> (flatten_infix lt)@[v]@(flatten_infix rt)

let flatten_prefix (t: 'a btree) : 'a list =
  let rec aux t =
    match t with
      | Leaf v -> [v]
      | Node(v,lt,rt) -> failwith "max_tree to be implemented"
  in aux t
*/

def flatten_infix[A] (t: Tree[A]) : List[A] = t match {
	case Leaf(v) => List(v)
	case Node(v,lt,rt) => flatten_infix(lt):::List(v):::flatten_infix(rt)
}

def flatten_prefix[A](t: Tree[A]) : List[A] = t match {
  case Leaf(v) => List(v)
  case Node(v,lt,rt) => List(v):::flatten_prefix(lt):::flatten_prefix(rt)
}

def qn_6_a[A] (t:Tree[A]) = flatten_infix[A] (t)
def qn_6_b[A] (t:Tree[A]) = flatten_prefix[A] (t)

// println(qn_6_a(Node(4,Leaf(1),Leaf(6))))
// println(qn_6_a(Node('a',Node('b',Leaf('d'),Leaf('e')),Leaf('c'))))
// println(qn_6_a(Node(1.4,Leaf(2.3),Node(3.5,Leaf(4.2),Leaf(5.6)))))
// println(qn_6_a(Leaf()))

// println(qn_6_b(Node(4,Leaf(1),Leaf(6))))
// println(qn_6_b(Node('a',Node('b',Leaf('d'),Leaf('e')),Leaf('c'))))
// println(qn_6_b(Node(1.4,Leaf(2.3),Node(3.5,Leaf(4.2),Leaf(5.6)))))
// println(qn_6_b(Leaf()))

/* 
  Q7 : 

       The above code below merely expresses the fact that
         power x 0 = 1
         power x n = x * (power (n-1))

       The above function is NOT tail-recursive.
       Can you write a tail-recursive
       version of this function which would accumulate its
       result in a 3rd paramater, called acc?

let power2 (x:int) (n:int) : int =
  let rec aux x n acc = 
    failwith "power2 is yet to be implemented"
  in aux x n 1
*/

def power2 (base:Int) (exp: Int): Int = {
   power_rec (base) (exp) (1)
}

def power_rec (base:Int) (exp: Int) (acc:Int):Int = {
  if (exp <=0) acc
  else power_rec (base) (exp-1) (acc * base)
}

def qn_7 (base:Int) (exp: Int): Int = power2 (base) (exp)

// println(qn_7 (2) (3))
// println(qn_7 (2) (10))
// println(qn_7 (-2) (3))
// println(qn_7 (2) (-3))
// println(qn_7 (100) (1))
// println(qn_7 (100) (0))

/* 
  Q8 : 

       We can also get a logarithmic-time function using

         power x 0 = 1
         power x (2*n = power (x^2) n
         power x (2*n+1) = x*(power (x^2) n)

       Implement such a function tail-recursively.
       How does this compare with the cryptic version of the code
       shown in Lecture 1.

 let power3 (x:int) (n:int) : int =
  let rec aux x n acc = 
    failwith "power3 is yet to be implemented"
  in aux x n 1

*/

def power3 (base:Int) (exp:Int):Int = {
  power_log (base) (exp) (1)
}

def power_log (base:Int) (exp:Int) (acc:Int) :Int = {
  if (exp <= 0) acc
  else if (exp %2 == 0) power_log (base*base) (exp/2) (acc)
  else power_log (base) (exp-1) (acc*base)
}

def qn_8 (base:Int) (exp: Int): Int = power3 (base) (exp)

// println (qn_8 (2) (10))
// println (qn_8 (5) (20))
// println(qn_8 (-5) (20))
// println(qn_8 (2) (-10))
// println(qn_8 (100) (1))
// println(qn_8 (100) (0))


/* 
  Q9: Last via List.fold_Left
	Consider the last function after.
	Re-implement it using fold_left.
*/

def last3[A](xs: List[A]) = xs match{
	case Nil => "Empty List"
	case x::xs => xs.foldLeft (x)((b,a)=> a)
}

def qn_9[A] (xs: List[A]) = last3 (xs)

// println(qn_9 (List(1,2,3,4,5)))
// println(qn_9 (List("a","bb","ccc")))
// println(qn_9 (List()))

/* 
  Q10: You can compute the average of a list of
	numbers by dividing the sum of the elements by
	the length of the list. Use a single fold_left to
	compute both these values, and then compute
	the average. Throw an exception if the list is empty.

(* replace failwith by your code *)
let average (xs: int list) : float =
	let (s,len) = List.fold_left ( fun (s_rec, l_rec) x -> (x+s_rec, 1+l_rec) ) (0,0) xs in
	(float_of_int s) /. (float_of_int len) 

*/

def average (xs: List[Int]): Double = xs match {
	case Nil => throw new Exception ("Empty List")
	case y::ys => 
	var (sum, length) = xs.foldLeft (0.0, 0.0) ( (acc,current) => (acc._1+current, acc._2+1) )
	return sum / length
}

def qn_10 (xs:List[Int]) : Double = average (xs)

// println(qn_10(List(1,2,3,4,5)))
// println(qn_10(List(3,3,3,3,3)))
// println(qn_10(List(5,10,23,41)))
// println(qn_10(List()))

/* 
  Q11 : Higher-Order functions for Trees

	You have designed a new tree data structure.
	It is a good practice	to provide a set of higher-order functions.

	(i) Based on your understanding of List.map, implement
		  a corresponding version for the map_tree function. 

	(ii) Similarly, based on your understanding of List9.fold_right, implement
		   a corresponding version for the fold_tree function. 

	Some examples of their uses are given below. They may be
	used as test cases for your implementation.
*/

def map_tree[A,B] (f: A => B) (t: Tree[A]) : Tree[B] = t match{
	case Leaf(v) => Leaf(f(v))
	case Node(v, lt, rt) => Node(f(v), map_tree (f) (lt), map_tree (f) (rt))
}

def fold_right_tree [A,B] (f: A => B) (g: (A,B,B) => B) (t:Tree[A]) : B = t match {
	case Leaf(v) => f(v)
	case Node(v, lt, rt) => g( (v), (fold_right_tree (f) (g) (lt)), (fold_right_tree (f) (g) (rt)))
}

def qn_11_a[A,B] (f: A => B) (t: Tree[A]) = map_tree (f) (t)
def qn_11_b[A,B] (f: A=> B) (g: (A,B,B) => B) (t: Tree[A]) : B = fold_right_tree (f) (g) (t)

// println(qn_11_a ((x:Int) => x + 1) (Node(4,Leaf(1),Leaf(6))))
// println(qn_11_a ((x:String) => x.concat(x) ) (Node("a",Node("b",Leaf("d"),Leaf("e")),Leaf("c"))))
// println (qn_11_b ((a:Int) => 1) ((a:Int,b: Int,c : Int) => 1 + b + c) (Node(4,Leaf(1),Leaf(6))))
// println (qn_11_b ((a:Int) => a*2) ((a:Int,b: Int,c : Int) => (a*2) + b + c) (Node(4,Node(1,Leaf(2),Leaf(3)),Leaf(6))))


/*
type 'a tree = 
	| Leaf of 'a
	| Node of 'a * ('a tree) * ('a tree);;

let t1 = Node (3,Leaf 1, Leaf 2);;
let t2 = Node (4,t1,t1);;
let t3 = Node (5,t2,t1);;

let rec map_tree (f:'a -> 'b) (t:'a tree) : 'b tree =
	failwith "to map a function to each node and leave"
(* 
   map_tree f (Node a1,Leaf a2,Leaf a3) 
    ==> Node (f a1, Leaf (f a2), Leaf (f a3))
*)

let fold_tree (f1:'a->'b) (f2:'a->'b->'b->'b) (t:'a tree) : 'b =
	failwith "to reduce a tree with f1,f2 to a value of output 'b type"
(* 
   fold_tree f1 f2 (Node a1,Leaf a2,Leaf a3) 
    ==> f2 a2 (f1 a1) (f1 a1)
*)

let t4=map_tree (fun x -> 2*x) t3;;
(* expecting a doubled version of t3
   Node (10, Node (8, Node (6, Leaf 2, Leaf 4), Node (6, Leaf 2, Leaf 4)),
     Node (6, Leaf 2, Leaf 4))
*)
fold_tree (fun x -> x) (fun a b c -> a+b+c) t3;;
(* expecting 27 *)
fold_tree (fun x -> [x]) (fun a b c -> b@(a::c)) t1;;
(* in-order traversal [1; 3; 2] *)
fold_tree (fun x -> [x]) (fun a b c -> a::(b@c)) t1;;
(* pre-order traversal [3; 1; 3] *)
*/

}
