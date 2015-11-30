(* 
  Tutorial 6 : OCaml Classes, Typing and Memoization
  Week of 5th October 2015
*)

(* 
   Q1. Class are factory generator of objects.
       Consider the counter class below.
       What are the expected values of the two counters
       c1 and c2 after the for-loop fragment?


       Answer: 	c1 = 20
    			c2 = 120

		Both c1 and c2 are of type "counter"; since they're objects of class counter

	*)
class counter init =
object (self) 
val mutable cnt = init;
method inc = cnt <- cnt+1;
method get = cnt;
end;;

let c1 = new counter 0;;
let c2 = new counter 100;;

for i=1 to 20 do
	c1 # inc;
	c2 # inc
done;;

let start_question no =
	begin
		Printf.printf ("==============\n");
		Printf.printf ("Question : %d\n") no;
		Printf.printf ("==============\n")
	end;;

	start_question 1;;
	Printf.printf ("Counter 1 : %d\n") (c1 # get);;
	Printf.printf ("Counter 2 : %d\n") (c2 # get);;

(* 
   Q2. Another way of generating objects is through
       functions, as shown below.
   Does c3 and c4 has the same type as C1 and C2?
   For example, using Ctrl-C Ctrl-T 
       on c1 gives: type: counter
       on c3 gives: type: < get : int; inc : unit >
   Are they the same type?
   How can you confirm this?
   		To confirm, do type annotations on both c1 and c3 as such:
		let c1:<get:int;inc:unit> = new counter 0;;
		or let c1:<inc:unit;get:int> = new counter 0;;
   		let c3:counter = gen_counter 0;;
   				Note that the order of the structure (of the visible methods) is not important
   				However, the name of the methods is important, therefore the below is not correct:
   				c1:<inc:unit;get1:int> = new counter 0;; will be deemed as wrong


   The difference between this and the method above is that this uses a function.
   It does not create a class to definte the counter. 

   The type of c3 and c4 are objects, that has the methods <get:int, inc: unit>
   type of the method gen_counter is: int -> <get:int, inc:unit>

   They are therefore different from c1 and c2 in terms of type.
   	This is refered to as structural typing (since it's not based on the
   	name as it does NOT have a name) and it's based on the visible method of the class
   	Note that private methods will not show on the type example is shown below the reset method

   	All fields are private.


   	Row polymorphism: where the type annotation specifies that the variable needs to have at least a certain 
   	amount of methods as such: let c5: <get:int;inc:unit:..> = counter 0;;
   	States that the c5 must have at least get and inc
   *)

let gen_counter i = 
	object (self) 
	val mutable cnt = i;
	method inc = cnt <- cnt+1;
	method get = cnt;
	method private reset = cnt <- 0
end;;

let c3 = gen_counter 0;;
let c4 = gen_counter 100;;

for i=1 to 20 do
	c3 # inc;
	c4 # inc
done;;

start_question 2;;
Printf.printf ("Counter 3 : %d\n") (c3 # get);;
Printf.printf ("Counter 4 : %d\n") (c4 # get);;

(* 
   Q3. 
   Can you confirm that both c1,c3 are the
   same types using the following print_counter 
   method. What is the type of this method?
*)

let print_counter (c:counter) =
	Printf.printf ("Counter : %d\n") (c # get);;

	start_question 3;;
	print_counter c1;;
	print_counter c3;;

(* 
   Q4. Consider an extension of the counter class.
   What is the type of this counter_reset class?
   		int->int-><reset:unit;inc:int>

   Is this strictly a subtype extension of counter?
   Can you use the call (print_counter c5)?
   		No
   Can you explain why you cannot do so?
		the method print_counter takes in a type (c:counter)
		The type annotation ":counter" specifies that the input must
		be exactly of type counter or equivalently (<get:int;inc:unit)
		However, c5 is of type counter_reset or equivalently (get:int;init:unit;reset:unit)
		Therefore, it will not accept c5 
		To resolve this, we can add a "#" as such print_counter (c:#counter)
		The # means that it can be a counter type, or an extension of it
		Alternatively, we can declare it using the row polymorphism as shown below:
		print_counter (c:<get:int;inc:unit:..>) or just print_counter (c:<get:int;..>)

	*)		

class counter_reset init tick =
object (self) 
inherit counter init as super
method reset = cnt <- 0
method inc = cnt <- cnt+tick;
end;;

let c5 = new counter_reset 0 2;;
let c6 = new counter_reset 100 1;;

for i=1 to 20 do
	c5 # inc;
	c6 # inc
done;;

Printf.printf ("Counter 5 : %d\n") (c5 # get);;
Printf.printf ("Counter 6 : %d\n") (c6 # get);;

(* print_counter c5;; *)


(* 
   Q5. One way to specify the sub-class
   extension is #counter which denotes a sub-type
   of the counter class.

   Would such a #counter type annotation allow you 
   to print values of c1, c3 and c5?
*)

let print_counter2 (c:#counter) =
	Printf.printf ("Counter = %d\n") (c # get);;


	start_question 5;
	print_counter2 c1;;
	print_counter2 c3;;
	print_counter2 c5;;


(* 
   Q6. If you do not use type annotation, what would
   be the type inferred by the following print_counter2?
   An example of this is shown below:
   Is this the most general class type for the given method?
*)

let print_counter3 (c) =
	Printf.printf ("Counter = %d\n") (c # get);;

	start_question 6;
	print_counter3 c1;;
	print_counter3 c3;;
	print_counter3 c5;;


(* 
   Q7. Consider the following generic class that
   can be used for general-purpose memomization.

   How would you use it to support the memoizing
   of the fib1 method below?

	['a,'b] memo)table : <get:'a -> 'b option; store: 'a -> 'b -> unit>

	Note that the above is before editing the store method to return 'b

*)

class ['a,'b] memo_table =
object (self) 
val tab = Hashtbl.create 10
method get (a:'a) =
	try 
	Some(Hashtbl.find tab a)
with _ -> None
method store (a:'a) (b:'b): 'b =
	(Hashtbl.replace tab a b; b)
end;;

let fib n =
	let rec aux n =
	if n<=1 then 1
else aux (n-1) + aux(n-2) in
aux n

let fib1 n =
	let mem = new memo_table in
	let rec aux n =
	match (mem # get n) with
	| Some v -> v
	| None -> 
	if n<=1 then mem # store n 1
else mem # store n (aux (n-1) + aux (n-2))
in
aux n

let print_fib fib n =
	begin
		Printf.printf ("Fib %d = %d\n") n (fib n);
		flush stdout
	end;;

	start_question 7;
	print_fib fib1 10;;
	print_fib fib1 40;;
	print_fib fib1 50;;
	print_fib fib1 100;;

(* 
   Q8. Let us now extend the memo_table class to
   include a higher-order method that is capable of
   memoizing arbitrary function. This should make this
   method much easier to use.

   We can extend the class memo-table with a new
   compute method with type: ('a->'b)->'a-> 'b.

   Implement this method, and then show that you can
   use it in the fib2 method below.

	method get (a:'a) =
		try 
		Some(Hashtbl.find tab a)
	with _ -> None
	method store (a:'a) (b:'b): 'b =
		(Hashtbl.replace tab a b; b)


	*)

class ['a,'b] memo_table_ho =
object (self) 
inherit ['a,'b] memo_table as super
method compute (f:'a->'b) (a:'a) : 'b =
	match (self # get a) with
	| Some v -> v
	| None -> self # store a (f a)
end;;

let fib2 n =
	let mem = new memo_table_ho in
	let rec aux n =
	mem # compute
	(fun n -> if n<=1 then 1
	else aux (n-1) + aux(n-2)) n
in aux n;;

start_question 8;
print_fib fib2 10;;
print_fib fib2 40;;
print_fib fib2 50;;
print_fib fib2 100;;
