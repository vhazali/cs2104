(* 
   Tutorial 1 : Introduction to OCaml
   
   Week of 24th August 2015

   Use: ocamlc -annot tut1.ml
   (to compile into an executable a.out, and a type annotation file)
   
   Reference on Ocaml (Real-World OCaml):
      https://realworldocaml.org/

   Emacs cheatsheet with Ocaml mode:
      https://www.lri.fr/~conchon/IPF/fiches/tuareg-mode.pdf
*)

(* 
 Q1. Consider the expression below
*)
 
  let x = 2 in
  let y = 3 in
  let x = x * 4 in
  print_endline ("Q1 Ans = "^(string_of_int (x+y))) ;;
  

(* Which x is being referenced in the last line?
   What output will be printed? *)

(*
Q2. Consider the expression below
*)
 
  let x = 2 in
  let y = let x = x * 4 in 3*x in
  print_endline ("Q2 Ans = "^(string_of_int (x+y))) ;;

(* Which x is being referenced in the last line?
   What output will be printed? *)
  
(* Q3. Consider the function below *)
 
  let foo x = x+1
   
(* What is the type of this function? *)

(* Q4. Consider the function below *)
 
  (* uncurried function *)
  let goo (x,y) = x
   
(* What is the type of this function? *)


(* Q5. Consider the function below *)
  (* curried function - Haskell B Curry *)
  let hoo x y = x
   
(*  
    What is the type of this function? 
	Does this function has the same type as ggo?
	How are the two functions related?
*)


(* Q6. Consider the maxlist function below *)
 
   let rec maxlist (xs: int list) =
     match xs with
	 | [x] -> x
	 | x::ys -> 
		let m2 = maxlist ys in
		if x>m2 then x
		else m2 
	 | [] -> failwith "empty list"
 ;;
		
   print_endline ("Q6 Ans (maxlist [3;16;1] = "^(string_of_int (maxlist [3;16;1]))) ;;
   print_endline ("hello");;

(* 
	(i) What is the type of this function?
	(ii) Explain what happen when an empty list is supplied as the input?
    (iii) Is it possible for recursive maxlist ys call to throw an exception?	
*)

(*
  Q7 : Rewrite maxlist function to use an auxiliary recursive
       function, as follows:
*)

   let maxlist2 xs =
     let rec aux xs sofar =
       match xs with
	 | [] -> sofar
	 | x::ys -> aux ys (max x sofar)
               (* failwith "to be completed" *)
     in match xs with
       | [] -> failwith "empty list"
       | x::ys -> aux ys x

(*
   How is this function different from the version in Q6
   Is it any better?
*)

(*
  Q8 : The maxlist function currently has type: int list -> int.

  Rewrite it to a version that is based on the following option
  type from the Pervasive standard library:

     type 'a option = None | Some of 'a

*)

   let rec maxlist3 (xs:int list) : int option =
     match xs with
       | [x] -> Some x
       | x::ys -> 
	     let m2 = maxlist3 ys in
             begin
               match m2 with
                 | Some m2 ->
	               if x>m2 then Some x
	               else Some m2 
                 | None -> Some x
             end
       | [] -> None

   ;;

   let maxlist4 xs =
     let rec aux xs sofar =
       match xs with
	 | [] -> sofar
	 | x::ys -> aux ys (max x sofar)
               (* failwith "to be completed" *)
     in match xs with
       | [] -> None
       | x::ys -> Some (aux ys x);;
