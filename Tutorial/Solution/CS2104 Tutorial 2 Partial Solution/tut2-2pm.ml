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
  else 1+(fib(n-1))+(fib(n-2));;

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
    else let (cnt,a,b)=aux(n-1) in (cnt+1,a+b,a)
  in (aux n);; 

let print_fib2 () =
  for i=1 to 200 do
    let (cnt,ans,_)=fib2 i in
    let _ = Printf.printf "Fib(%d) = %d using %d calls\n" i ans cnt in
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
    if n<=1 then (num_of_int 1,num_of_int 0)
    else let (a,b)=aux(n-1) in (a +/ b,a)
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
let fib4 n =
  let rec aux i r1 (* fib i *) r0 (* fib i-1 *)  =
    if i=n then r1
    else aux (i+1) (r1+r0) (* fib i+1*) r1 (* fib i *)
  (* failwith "to be implemented with recursive (aux (n-1))" *)
  in aux 0 1 (* fib 0 *) 0 (* fib -1 *);; 

let print_fib4 () =
  for i=1 to 60 do
    let _ = Printf.printf "Fib(%d) = %d \n" i (fib4 i) in
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

(* let pr_opt f ov = *)
(*   match ov with *)
(*     | None -> "None" *)
(*     | Some v -> let s = f v in *)
(*       "Some("^s^")";; *)

(* pr_opt string_of_int r *)


let rec fact n =
	if n==0 then num_of_int 1
	else (num_of_int n) */ (fact (n-1));;

let rec factlist n =
	if n==0 then []
	else (fact n)::(factlist (n-1));; 

let cnv xs = List.map string_of_num xs;;
 
(* let factlist (n:int) : Num.num list = *)
	(* failwith "to be implemented using tupled recursion and Num library" *)
	
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
      (* rs@ *) []::(add e rs)
  in aux xs;;

let enum_suffix xs = 
  let rs = List.rev xs in
  let ans = enum_prefix rs in
  List.map List.rev ans;;

enum_prefix [1;2;3;4];;

enum_prefix [4;-1;2;-9;3];;

(* 
  Q7: Using enum_prefix, write a function
      that would find the largest prefix in a given list.
   What is the time-complexity of this method?

*)
let rec sum xs =
  match xs with
    | [] -> 0
    | x::xs -> x+(sum xs)

let choose_max xs1 xs2 =
  let s1= sum xs1 in
  let s2= sum xs2 in
  if s1<s2 then xs2
  else xs1;;

let rec find_max rs =
  match rs with
    | [a] -> a
    | b::ds -> choose_max b (find_max ds);;

let max_prefix xs = 
  let rs = enum_prefix xs in
  find_max rs 

  (* failwith "find largest seq in rs";; *)

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
