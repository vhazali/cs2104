(*******************************************)
(* Lab/Tutorial 3 : Higher-Order Functions *)
(*******************************************)
(*  Week of 7 Sept *)

(* 
  Q1: Last via List.fold_Left

Consider the last function below.
Re-implement it using fold_left.
*)

let last (xs:'a list) : 'a =
  let rec aux z xs =
    match xs with
    | [] -> z
    | y::ys -> aux y ys in
  match xs with
  | [] -> failwith "no last element"
  | x::xs -> aux x xs

let f z y = y

(* replace failwith by your code *)
(* note that z is the accumulator for tail-recursive method *)
let last2 (xs:'a list) : 'a =
  match xs with
  | [] -> failwith "no last element"
  | x::xs -> List.fold_left (fun z y -> y) (x) xs
	(* failwith "to be implemented using List.fold_left" *)

let fold_left (f:'b -> 'a ->'b) (z:'b) (xs:'a list) : 'b  =
  let rec aux z xs =
    match xs with
    | [] -> z
    | y::ys -> (aux (f z y) ys)
  in aux z xs;;

(* 
  Q2 : Sorting
	
	Consider the insertion sort method below.	
	Re-implement the two methods using List.fold_right.
*)


let rec insert x ys =
  match ys with
  | [] -> [x]
  | y::ys -> 
    if x<=y then x::y::ys
    else y::(insert x ys);;

let fold_right f xs z =
  let rec aux xs =
    match xs with
    | [] -> z
    | y::ys -> f y (aux ys)
  in aux xs;;

  (* f y (r,ys) =  *)
  (*   if x<=y then (x::y::ys,y::ys) *)
  (*   else (y::r,y::ys) *)



(* We return the sorted list and original list ys *)
(* Can you prove that in the base-case ys=tail of sorted list? *)
(* If so, you can optimize further by returning only one outcome*)
(* replace failwith by your code *)
let insert2 x ys =
  fst (List.fold_right 
      (fun y (r,ys) -> 
          if x<=y then (x::y::ys,y::ys)
          else (y::r,y::ys)) 
      ys ([x],[]))
  (* failwith "to be implemented using List.fold_right" *)

let rec sort xs =
  match xs with
  | [] -> []
  | y::ys -> insert y (sort ys);;


let sort2 xs =
  List.fold_right (insert) xs ([])
(* failwith "to be implemented using List.fold_right" *)



(* 
  Q3 : You can compute the average of a list of
	numbers by dividing the sum of the elements by
	the length of the list. Use a single fold_left to
	compute both these values, and then compute
	the average. Throw an exception if the list is empty.
*)

(* replace failwith by your code *)
let average (xs: int list) : float =
  let sum = List.fold_left (+) 0 xs in
  let len = List.fold_left (fun z y -> z+1) 0 xs in
  (float_of_int sum) /. (float_of_int len);;

let average (xs: int list) : float =
  let (sum,len) = List.fold_left 
    (fun (z_sum,z_len) y -> (z_sum+y,z_len+1)) (0,0) xs in
  (float_of_int sum) /. (float_of_int len);;

(* this function will help sum up alternative elements of a list
   It uses i as index position of the list element *)
let sum_alt xs = List.fold_left 
  (fun (i,s) y -> 
      if i mod 2 ==0 then (i+1,y+s)
      else (i+1,s)) (0,0) xs

  (* failwith "average to be computed with a single fold_left" *)


(* What does this function do? *)
(* It adds the index position into the list element *)
let add_num xs = 
  let (_,lst) = List.fold_left (fun (n,lst) y -> (n+1,lst@[(n,y)])) (0,[]) xs 
  in lst;;

(* 
  Q4 : Using Pipeline
	
	You can compute the median of a list of
	integers by sorting the list and computing its
	length, and then finding a middle element in
	the list. If there is an even number of elements,
	you are expected to compute the average of middle two
	elements. You are to use the |> operator as
	below in your median method.
*)

let ( |> ) (x:'a) (f:'a->'b) : 'b =  f x;;

 (* xs  *)
 (*  |> f1 *)
 (*  |> f2 *)

 (* = f2 (f1 xs) *)

(* your implementation for mid need not use higher-order functions *)
let rec mid (xs:int list) : float 
  (* pre: input list is sorted *)
  = xs 
  |> List.length
  |> (fun n ->
         let mid = n/2 in
         if n mod 2 ==0 then 
           let r1 = List.nth xs (mid-1) in
           let r2 = List.nth xs (mid) in
           (float_of_int (r1+r2)) /. 2.0
         else float_of_int (List.nth xs mid));;

  (* failwith "to return the median value of a sorted list" *)

let median xs =
  xs 
  |> sort
  |> mid

let median xs =
  mid (sort xs) 

let median xs =
  let r1 = (sort xs) in
  mid r1


(* 
  Q5 : Higher-Order functions for Trees

	You have designed a new tree data structure.
	It is a good practice	to provide a set of higher-order functions.

	(i) Based on your understanding of List.map, implement
		  a corresponding version for the map_tree function. 

	(ii) Similarly, based on your understanding of List9.fold_right, implement
		   a corresponding version for the fold_tree function. 

	Some examples of their uses are given below. They may be
	used as test cases for your implementation.

*)

type 'a tree = 
  | Leaf of 'a
  | Node of 'a * ('a tree) * ('a tree);;

let t1 = Node (3,Leaf 1, Leaf 2);;
let t2 = Node (4,t1,t1);;
let t3 = Node (5,t2,t1);;

(* map for tree *)
let rec map_tree (f:'a -> 'b) (t:'a tree) : 'b tree =
  match t with
    | Leaf x -> Leaf (f x)
    | Node (x,lt,rt) -> Node (f x,(map_tree f lt),(map_tree f rt))


(* fold for tree *)
  (* failwith "to map a function to each node and leave" *)
(* 
   map_tree f (Node a1,Leaf a2,Leaf a3) 
    ==> Node (f a1, Leaf (f a2), Leaf (f a3))
*)

let fold_tree (f1:'a->'b) (f2:'a->'b->'b->'b) (t:'a tree) : 'b =
  let rec aux t =
    match t with
      | Leaf x -> f1 x
      | Node (x,lt,rt) -> f2 x (aux lt) (aux rt) 
  in aux t

let map_tree (f:'a -> 'b) (t:'a tree) : 'b tree =
  fold_tree (fun x -> Leaf (f x)) 
      (fun x lt rt -> Node (f x,lt,rt)) t

(* failwith "to reduce a tree with f1,f2 to a value of output 'b type" *)
(* 
   fold_tree f1 f2 (Node a1,Leaf a2,Leaf a3) 
    ==> f2 a1 (f1 a2) (f1 a3)
   fold_tree f1 f2 (Node a1,Leaf a2,Leaf a3) 
    ==> Node (2,a1) (Leaf (1,a2)) (Leaf (3,a3))
*)

(* summing elements of tree *)
let sum_tree t = fold_tree (fun x -> x) (fun x sum_lt sum_rt -> x+sum_lt+sum_rt) t

let count_tree t = fold_tree (fun x -> 1) (fun x lt rt -> 1+lt+rt) t

let count_leaves t = fold_tree (fun x -> 1) (fun x lt rt -> lt+rt) t

let height_tree t = fold_tree (fun x -> 1) (fun x lt rt -> 1+(max lt rt)) t

let max_tree t = fold_tree (fun x -> x) (fun x lt rt -> max x (max lt rt)) t


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


(* Question : How about post-order traversal *)


(* A first-order function to add in-order index to element of tree *)
(* Can a higher-order function do this? *)
let rec num_tree t n =
  match t with
    | Leaf x -> (Leaf (n,x), 1)
    | Node (x,lt,rt) -> 
          let (l_t,l_n) = num_tree lt n in
          let p = (n+l_n,x) in
          let (r_t,r_n) = num_tree rt (n+l_n+1) in
          (Node (p,l_t,r_t),1+l_n+r_n)

  (* fold_tree (fun x -> (Leaf (n,x),1))  *)
  (*     (fun x (lt,nl) (rt,nr)  *)
  (*       -> (Node (n,x), , 1+nl+nr) *)

(* 
  Q6 : Writing one higher-order function in terms of another.
	
Implement List.map, List.filter and List.partition
in terms of LIst.fold_right.

	 
*)

(* map in terms of fold_right *)
let map2 f xs = List.fold_right (fun x z -> (f x)::z) xs []

(* How about map in terms of fold_left *)
let map3 f xs = failwith ("List.fold_left (fun z x -> ..) (..) xs")


