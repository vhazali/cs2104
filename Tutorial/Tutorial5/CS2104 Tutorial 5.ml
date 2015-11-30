(* CS2104 Tutorial 5 : Imperative programmiong *)
(* Lab Session Practice for Week 28 Sept 2015 *)

(* 
   You are required to submit what you have done 
   This aasignment will not be graded, but you will be given
   participation point. Please submit by 
      Friday 2 Oct 2015 6pm.
    *)

(* 
  Q1 : Consider a tail-recursive method.
  Re-implement this method using an iterative loop together
  with two imperative ref type values that are internal to the method 
*)

let fib n =
  let rec aux n a b =
  if n<=0 then (a,b)
else aux (n-1) (a+b) a
in fst(aux n 1 0);;

let fib_imp n =
  let a = ref 1 in
  let b = ref 0 in
  for i = 1 to n do
   let old_a = !a in
    a := old_a + !b;
    b := old_a;
  done;
!a;;

let fib_while n = 
  let a = ref 1 in
  let b = ref 0 in
  let mutable_n = ref n in
  while !mutable_n > 0 do
    let old_a = !a in
    mutable_n := !mutable_n -1;
    a := old_a + !b;
    b := old_a;
  done;
!a;;

(* 
   Q2 : Would you consider your implementation as a pure function?
   Does it have any side-effects?
 *)


(*
 f x = if base x then const
       else H(f (nxt x)))
      
*)

(* 
   Q3 : The for-loop is implemented as follows.
   What is the polymoprhic type of this method?
 *)

let for_loop init final stmt =
  let rec aux i =
  if i<=final then (stmt i; aux (i+1))
else ()
in aux init

(* implementing the fib with the self-defined for loop: *)
let fib_imp2 n =
  let a = ref 1 in
  let b = ref 0 in
  for_loop 1 n 
    (fun i -> 
      let old_a = !a in 
      a := old_a + !b;
      b := old_a;);
  !a;;


(* 
   Q4 :  Write two higher-order methods that would
 implement a for-down-to loop iterator, and a while loop method *)

let rec while_loop pred body =
  if pred() then
    begin
      body ();
      while_loop pred body
    end
  else ()
;;

(* implementing fib with self-defined while loop: *)
let fib_while2 n = 
  let a = ref 1 in
  let b = ref 0 in 
  let mutable_n = ref n in
  while_loop (fun () -> !mutable_n > 0) 
  (fun () -> let old_a = !a in
    mutable_n := !mutable_n -1;
    a := old_a + !b;
    b := old_a;);
  !a;;

let for_down_loop init final stmt =
  let rec aux i = 
    if i >= final then (stmt i; aux (i-1))
  else ()
in aux init
;;

(* 
   Q5 :  Explain why iterators are not being implemented
   as functions in conventional languages, such as C or Java? 
 *)


(* 
   Q6 : Implement fib function with memoization 
   using a hash table.Write two higher-order methods that would
   implement a for-down-to loop iterator, and a while loop method.

   How does this compares with your implementation in Q1?
 *)

let fib8 n =
  let h = Hashtbl.create 10 in
  let rec aux n =
  try 
    Hashtbl.find h n
  with _ ->
    let r = 
      if n <= 1 then 1 
      else 
        aux (n-1) + aux (n-2)
    in Hashtbl.add h n r; r
in aux n;;


(* counting number of calls *)
(* pure method: 

  aux n cnt = 
    let cnt = cnt + 1 in
      if n<=1 then (1,cnt)
      else 
        let (r1, cnt2) = aux (n-1) cnt in
        let (r2, cnt3) = aux (n-2) cnt2 in
        (r1+r2, cnt3)
*)

(* imperative method:

let cnt = ref 0 in 
let rec aux n =
  cnt := !cnt+1;
  if n<=1 then 1
  else 
    aux (n-1) + aux (n-2)
in (!cnt, aux n);; 

note that the order here is impt as it will evaluate the tuple from
right to left
*)



(* 
   Q7 : 
   Consider a ref type with an empty imperative list .

When compiling using ocamlc Tut5.ml, we got the following 
compile-time type errorf

File "Tut5.ml", line 89, characters 14-20:
Error: The type of this expression, '_a list ref,
       contains type variables that cannot be generalized

Explain why and how you may overcome this problem?


Answer:
Not possible for the ref to be polymorphic, and it must be mono-morphic type.

Therefore, one way to resolve it will be to give the list a type.
Example, we can give it a string list ref type as such: 
let (emp_lst: string list ref) = ref []
*)

let emp_lst = ref []
