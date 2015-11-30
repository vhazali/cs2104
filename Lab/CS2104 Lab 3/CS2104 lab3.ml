(* Please see Ocaml String library for relevant methods *)

(* memoisation table class *)
class ['a, 'b] memo_table =
	object (self)
	val tab = Hashtbl.create 10
method get (a:'a) =
	try 
	Some(Hashtbl.find tab a)
with _ -> None
method store (a:'a) (b:'b): 'b =
	(Hashtbl.replace tab a b; b)
end;;
class ['a, 'b] memo_table_ho =
	object(self)
	inherit ['a, 'b] memo_table as super
	method compute (f: 'a -> 'b) (a:'a):'b =
	match (self # get a) with
	| None ->
		let r = f a in(self # store a r); r
	| Some v -> v
end;;

(* Cases 
	i, j are the index of the two strings
	i = index of first string
	j = index of second string
	os1= first line of output == modified s1
	os2= second line of output == modified s2
	os3= third line of output == line showing - + or * depending on score of column
	ans= score of the solution

	Want to find out the best score from the alignment of strings from i and j

	So first we check if they're finished
		If both are finished, format the os1,os2,os3 into one string, then return score
		Else If i finished, j not finished, add spaces to os1, add * to os3, and add penalty for spaces to score
		Else If j finished, i not finished, add spaces to os2, add * to os3, and add penalty for spaces to score
		If none is finished, 3 cases
			1. Take both characters: aux (i+1) (j+1), add both chars to os1 and os2, add correct sign to os3, add score
			2. Take only c2: aux (i) (j+1), add space to os1, add c2 to os2, add * to os3, add penalty for spaces
			3. Take only c1: aux (i+1) (j)m add c1 to os1, add space to os2, add * to os3, add penalty for spaces
		Choose the maximum score of the three
*)
(* this method finds alignment between two strings
and print out a best solution *)
let align_print s1 s2 penalty_space =
	let n1 = String.length s1 in
	let n2 = String.length s2 in
	let maxS (a,m1) (b,m2) = if m1<m2 then (b,m2) else (a,m1) in
	let score a b = if a=b then 1 else -1 in 
	let string_of_char c = String.make 1 c in
	let memotab = new memo_table_ho in
	let rec aux i j (os1,os2,os3,ans) =
		memotab # compute (fun (i,j,ans) ->
		if i>=n1 then							(* Finished traversing s1 *)
			if j>=n2 then 						(* finished traversing both strings *)
				(os1^"\n"^os2^"\n"^os3^"\n"^"Score: "^string_of_int ans^"\n", ans)
			else								(* Still have chars in s2 *)
				let c2 = String.get s2 j in
				aux i (j+1) (os1^" ", os2 ^ string_of_char c2, os3 ^ "*", penalty_space + ans)
		else              
			if j>=n2 then   					(* Finished s2 but still have chars in s1 *)
				let c1 = String.get s1 i in
				aux (i+1) (j) (os1^string_of_char c1, os2 ^ " ", os3 ^ "*", penalty_space + ans)
			else            					(* Not finished with either strings *)
				let c1 = String.get s1 i in
				let c2 = String.get s2 j in
				(* Case 1: we compare the char in i with char in j *)
				let m = score c1 c2 in
				let c3 = if m = 1 then "+" else "-" in
				let m1 = aux (i+1) (j+1) (os1^string_of_char c1, os2^string_of_char c2, os3^c3, ans + m) in
				(* Case 2: we add a space to i*)
				let m2 = aux (i) (j+1) (os1^" ", os2^string_of_char c2, os3^"*", ans + penalty_space) in
				(* Case 3: we add a space to j*)
				let m3 = aux (i+1) (j) (os1^string_of_char c1, os2^" ", os3^"*", ans + penalty_space) in
				(* find the best result in these 3 cases *)
				maxS m1 (maxS m2 m3)) (i,j,ans)
	in let (str,res) = aux 0 0 ("","","",0) in print_string (str);;

(* Cases 
	i, j are the index of the two strings
	i = index of first string
	j = index of second string

	Want to find out the best score from the alignment of strings from i and j

	So first we check if they're finished
		If both are finished, score is 0, cause nothing to match (and return empty strings)
		Else If i finished, j not finished, add spaces to i, and add penalty for spaces
		Else If j finished, i not finished, add spaces to j, and add penalty for spaces
		If none is finished, 3 cases
			Score(c1,c2) + aux(i+1)(j1+1)
			Score (space, c2) + aux(i)(j+1)
			Score (c1, space) + aux(i+1)(j)
		Choose the maximum score of the three
*)
(* this method finds alignment where space is given a 
specific penalty *)
let align_w_penalty s1 s2 penalty_space =
	(* let n1 = String.length s1 in
	let n2 = String.length s2 in
	let max m1 m2  = if m1<m2 then m2 else m1 in
	let score a b = if a=b then 1 else -1 in 
	let string_of_char c = String.make 1 c in
	let memotab = new memo_table_ho in
	let rec aux i j =
		memotab # compute (fun (i,j) ->
		if i>=n1 then		(* Finished traversing s1 *)
			if j>=n2 then   0	(* finished traversing both strings *)
			else				(* Still have chars in s2 *)
				let ans = aux i (j+1) in
				penalty_space + ans
		else              
			if j>=n2 then   (* Finished s2 but still have chars in s1 *)
				let ans = aux (i+1) (j) in
				penalty_space + ans
			else            (* Not finished with either strings *)
				let c1 = String.get s1 i in
				let c2 = String.get s2 j in
				(* Case 1: we compare the char in i with char in j *)
				let m = score c1 c2 in
				let m1 = m + (aux(i+1)(j+1)) in 
				(* Case 2: we add a space to i and compare it to the char in j *)
				let m2 = penalty_space + (aux(i)(j+1)) in
				(* Case 3: we add a space to j and compare it to the char in i *)
				let m3 = penalty_space + (aux(i+1)(j)) in
				(* find the best result in these 3 cases *)
				max m1 (max m2 m3)) (i,j)
	in aux 0 0  *)
	align_print s1 s2 penalty_space;;

(* you are free to generalise methods below *)
let align s1 s2 =
	align_print s1 s2 (-2);;


