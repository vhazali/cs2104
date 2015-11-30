(* 
   Lab Assignment 1 : Declarative OCaml Examples
   
   Week of 24th August 2015

   Everyone should submit this assignment by 
        Wed 5pm. 2nd September 2015
   
   In case of problem, please consult the lecturer/tutors 
   either during tutorial or after lecture.
*)


(* 
  Q1 : Write a recursive function that would return the
       last element of the list. In case of empty list,
       throw a Failure exception.

       What is the polymorphic type of this function?

       Answer: last: 'a list -> 'a = <fun>
*)
let rec last (xs:'a list) : 'a = 
  match xs with 
  | [] -> failwith "Empty list provided"
  |[a] -> a
  | a::xs -> last (xs);;

(* 
  Q2 : Change the last function to one with the following
       type: 'a list -> 'a option
       This function should return Some v, where v is the
       last element of the list. If the list is empty, you
       should return None.
*)
let rec last_opt (xs: 'a list) : 'a option = 
  match xs with
  | [] -> None
  | [v] -> Some v
  | v::xs -> last_opt xs ;;

(* 
  Q3 : Write a recursive function that would return the
       last two elements of the lists as a pair of values.
       In case you have less than two elements, throw a Failure exception.

       What is the polymorphic type of this function?

       Answer: last_two: 'a list -> 'a * 'a = <fun>
*)
let last_two (xs:'a list) : 'a * 'a =
  let rev = List.rev xs in
  match rev with
  | [] -> failwith "Empty list provided"
  | [a] -> failwith "Only has one item in list"
  | a::b::xs ->(a,b)

(* 
  Q4 : Write a recursive function to sort a list of numbers
       using the insertion sort method.

       For your convenience, we have provided an
       insert procedure.
       (i) can you improve the insert method to
           avoid constructing (y::ys) in the base case?
           (Hint : use the as-pattern notation)
      (ii) implement a recursive sort method
*)

let rec insert x ys =
  match ys with
    | [] -> [x]
    | y::ys -> 
          if x<=y then x::y::ys
          else y::(insert x ys)

(* improved insert method:*)
let rec insertImproved x ys =
  match ys with 
  | [] -> [x]
  | y::ys as ns ->
    if x <= y then x::ns
    else y::(insert x ys)


let sort xs =
  match xs with
  |[] -> []
  | x::ys -> insert x (sort ys);;

(* 
  Q4 : Consider a uprim type to capture either 
       integer, float or a string value.

       You can build a list of mixed type using
       it, and can perform List.rev and List.length
       using it.

       Compute the sum of mixed list using the value_of_mix
       function.
*)
type uprim = I of int | F of float | S of string ;;

let mix_ls = [I 3; F 4.3; S "hello"; I 4];;

print_endline ("mix_ls has length "^(string_of_int (List.length mix_ls)));;
List.rev  mix_ls;;

let value_of_mix up =
  match up with
    | I v -> v
    | F v -> (int_of_float v) (* truncates the float value *)
    | S s -> (String.length s) (* length of string *)

let rec sum_of_mix_list (ms: uprim list) : int =
  match ms with
  | [] -> 0
  | [a] -> value_of_mix a
  | a::xs -> value_of_mix a + sum_of_mix_list xs;;

(* 
  Q5 : Let us define uprim using the basic sum type instead,
       and write functions that are isormoprhic to those
       found in Q4.
*)
type ('a,'b) sum = L of 'a | R of 'b;;

type uprim2 = (int,(float,string) sum) sum;;

let mk_I (v:int) = L v  (* makes an integer value *)
let mk_F (f:float) = R (L f) (* makes a float value *)
let mk_S (s:string) = R (R s) (* makes a string value *)

let mix_ls2 = [mk_I 3; mk_F 4.3; mk_S "hello"; mk_I 4];;

print_endline ("mix_ls2 has length "^(string_of_int (List.length mix_ls2)));;
List.rev  mix_ls2;;

let value_of_mix2 up =
  match up with
    | L v -> v
    | R (L v) -> (int_of_float v) (* truncates the float value *)
    | R (R s) -> (String.length s) (* length of string *)

let sum_of_mix_list2 (ms: uprim2 list) : int =
  match ms with
  | [] -> 0
  | [x] -> value_of_mix2 x
  | x::ys -> (value_of_mix2 x) + sum_of_mix_list2 ys;;


(* 
  Q6 : Consider a polymorphic tree.

       Write a function that will return the largest value in
       the tree. You may use the max function.
*)
let choose_max v1 v2 = 
  if v1 < v2 then v2
else v1

type 'aa btree = Leaf of 'aa | Node of 'aa * ('aa btree) * ('aa btree) ;;
let t1 = Leaf 3;;
let t2 = Node(4,t1,t1);;
let t2 = Node(6,t2,t1);;

let max (x,y) =
  if x < y then y else x;;

let rec max_tree (t: int btree) : int =
  match t with
  | Leaf x -> x
  | Node (x, L, R) -> max (x, (max(max_tree left, max_tree right)));;


(* 
  Q7 : Below is a function that will flatten a tree into a list
       by traversing the tree in an infix-order.

       Write another function that will flatten a tree 
       based on pre-fix traversal.
*)
let rec flatten_infix (t: 'a btree) : 'a list =
  match t with
    | Leaf v -> [v]
    | Node(v,lt,rt) -> (flatten_infix lt)@[v]@(flatten_infix rt)

let flatten_prefix (t: 'a btree) : 'a list =
    match t with
      | Leaf v -> [v]
      | Node(v,lt,rt) -> [v]@(flatten_prefix lt)@(flatten_prefix rt)

(* 
  Q8 : The power function takes two arguments x n so as
       to return x^n.

       An expected precondition is that n>=0
       Write an assertion statement to ensure that this pre-condition
       will always be met.

       What happens to your function if you had used
       a negative n value?

       Answer: function throws an exception to show that there was an assert failure
*)
let rec power (x:int) (n:int) : int =
  if n<0 then assert false
  else if n==0 then 1
  else x * (power x (n-1))


(* 
  Q9 : 

       The above code below merely expresses the fact that
         power x 0 = 1
         power x n = x * (power (n-1))

       The above function is NOT tail-recursive.
       Can you write a tail-recursive
       version of this function which would accumulate its
       result in a 3rd paramater, called acc, as shown below?

*)
let power_tail (x:int) (n:int) : int =
  let rec aux x n acc = 
    if n< 0 then assert false
    else if n == 0 then acc
    else aux (x) (n-1) (acc * x)
  in aux x n 1


(* 
  Q10 : 
       We can also get a logarithmic-time function using
         power x 0 = 1
         power x (2*n = power (x^2) n
         power x (2*n+1) = x*(power (x^2) n)
       Implement such a function tail-recursively.
*)
let power_logn (x:int) (n:int) : int =
  let rec aux x n acc = 
    if n < 0 then assert false
    else if n == 0 then acc
    else if n mod 2 == 0 then aux (x*x) (n/2) (acc*x)
    else aux (x*x) ((n-1)/2) (x*acc*x)
  in aux x n 1
