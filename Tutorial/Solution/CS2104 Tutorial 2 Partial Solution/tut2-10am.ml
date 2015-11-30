(* Tutorial 2 : Writing Efficient Recursion *)
(* Week of 31st August 2015 *) 
(* 
    OCaml Reading Resources 

    tutorial:
      http://ocaml.org/learn/tutorials/

    introduction:
      http://www.cs.jhu.edu/~scott/pl/lectures/caml-intro.html

    real world ocaml book:
       https://realworldocaml.org/v1/en/html/index.html

*)


(* 
  Q1: Load the naive fibonacci and a print method for a series of fibonacci.
At which method call did the interpreter paused for some time?
*) 

let rec fib n =
  if n<=1 then 1
  else (fib(n-1))+(fib(n-2));;

let print_fib () =  
  for i=1 to 40 do
    let _ = Printf.printf "Fib(%d) = %d \n" i (fib i) in
    flush(stdout)
  done;;  
 
(* 
  Q2: Load a more efficient version of fib2 below together with its
	print method. Take note of which call first has an integer overflow.
*)
let fib2 n =
  let rec aux n =
    if n<=0 then (1,1,0)
    else 
      let (cnt,a,b)=aux(n-1) 
      in (cnt+1,a+b,a)
  in let (c,ans,_) = aux n in
  (c,ans);;

let print_fib2a () =
  for i=1 to 200 do
    let (cnt,ans) = fib2 i in
    let _ = Printf.printf "Fib(%d) = %d in %d calls \n" 
      i ans cnt  in
    flush(stdout)
  done;;  
	
(* 
  Q3: To avoid integer overflow, you can either use the big_int module or
	the following num module .
	
	  type num = 
    |	Int of int
    |	Big_int of Big_int.big_int
    |	Ratio of Ratio.ratio

  http://caml.inria.fr/pub/docs/manual-ocaml/libref/Num.html
	
	Rewrite fib3 to use num type so that integer overflow does not occur.
	
	From top-level, first #load "nums.cma";; into memory.
	After that, open Num;;

  You may need to use:
	  num_of_int : int -> num
	  string_of_num : num -> string
		+/ : num -> num -> num		
		
	Run print_fib3 to get the first 100 fibs.
	
*)

open Num;; 
 
let fib3 (n:int) : Num.num =
  let rec aux n =
    if n<=0 then (num_of_int 1,num_of_int 0)
    else 
      let (a,b)=aux(n-1) 
      in (a +/ b,a)
  in fst(aux n);;
	(* failwith "to be implemented using Num.num_of_int and Num.(+/)" *)

let print_fib3 () =
  for i=1 to 200 do
    let _ = Printf.printf "Fib(%d) = %s \n" i (Num.string_of_num(fib3 i)) in
    flush(stdout)
  done;;  


(* 
  Q4: Implement fibonacci as a tail-recursive method with two
      parameters, initially denoting fib(0) and fib(1).
      Is is computational behaviour different from a Loop?

*)
let fib2 i = fst(fib2 i);;

let fib4 n =
  let rec aux n r1 r0 (*fib i, fib i-1*) =
    if n=1 then r1+r0
    else aux (n-1) (r1+r0) r1
(* failwith "to be implemented with recursive (aux (n-1))" *)
  in aux n 1 0;; 

let print_fib2 () =
  for i=1 to 60 do
    let _ = Printf.printf "Fib(%d) = %d \n" i (fib2 i) in
    flush(stdout)
  done;;  

(* 
  Q5: You have been asked to implement a list of factorials.
	A naive way of implementing it is given below. The naive
	algorithm has O(n^2) complexity.
	
  Can you write a more efficient tupled recursive method to do this
  in O(n) time?
	
  To avoid integer overflow, change it to use the num type.
*)

let rec fact n =
	if n==0 then 1
	else n * (fact (n-1));;

let rec factlist n =
	if n==0 then []
	else (fact n)::(factlist (n-1));; 
 
let factlist (n:int) : Num.num list =
	failwith "to be implemented using tupled recursion and Num library"
	
(* 
  Q6: Consider a function to enumerate all prefixes of
      a list of elements. Such a function can be written
      using recursion, as follows:
  
*)
let enum_prefix xs = 
  let rec add e xs =
    match xs with
    | [] -> []
    | r::rs -> (e::r)::(add e rs) in
  let rec aux xs =
    match xs with
    | [] -> [[]]
    | e::es -> 
      let rs = aux es in
      (* rs@ *)[]::(add e rs)
  in aux xs;;

enum_prefix [1;2;3;4];;

enum_prefix [4;-1;2;-9;3];;

(* 
  Q7: Using enum_prefix, write a function
      that would find the largest prefix in a given list.
   What is the time-complexity of this method?

*)
let max_prefix xs = 
  let rs = enum_prefix xs in
  failwith "find largest seq in rs";;

(* 
  Q8: Without using enum_prefix, write a
      more efficient max_prefix2 method that would
      return an identical result as max_prefix.

      Using recursion to implement your new method.
      Compare and contrast between these two 
      methods for computing max_prefix.

*)

let rec max_prefix2 xs = 
  failwith "return largest prefix of xs";;
