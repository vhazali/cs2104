(* Lecture 9 : Parser Combinator and Arithmetic Parser *)
open Globals;;

module Keyword =
struct
  (* permitted symbols *)
  let symbols = ["=";"++";"+";"-";"*";"/";"~";"(";")"];;
  (* reserved keywords in calculator *)
  let alphas = ["let";"in"];;
end

module Lexical =
struct
  open Gen.Basic
  (* permitted token types *)
  type token = Key of string (* reserved keyword/symbol *)
               | Id of string  (* identifier *)
               | NumI of Num.num;; (* numeric constant *)
  exception LexErr of string;;

  (* printer for token *)
  let string_of_token t =
    match t with
      | Key s -> "Key "^s
      | Id s -> "Id "^s
      | NumI n -> "NumI "^(Num.string_of_num n)

  (* letters *)
  let is_letter c =
    'A'<= c && c <= 'Z'
    || 'a'<= c && c <= 'z';;

  (* digits *)
  let is_digit c =
    '0'<= c && c <= '9';;

  (* letters or digits *)
  let is_letter_or_digit c =
    is_letter c || is_digit c;;

  (* special symbols permitted *)
  let specials = explode "!@#$%^&*()+-=[]:/\"|;'\\,./?`_~<>";;

  (* alphanumeric *)
  (* pre: id starts with an alphabet *)
  let rec alphanum (cs:char list) (id:char list) : (string * char list) =
    match cs with
      | c::cs2 ->
            if is_letter_or_digit c 
            then alphanum cs2 (c::id)
            else (implode (List.rev id),cs)
      | [] -> (implode (List.rev id),[]);;
  let alphanum (cs:char list) (id:char list) =
    Debug.no_2 "alphanum"
        implode implode 
        (pr_pair pr_id implode) 
        alphanum cs id

  (* TO BE IMPLEMENTED *)
  (* numeric integer of arbitrary precision *)
  (* type: char list -> char list -> Num.num * char list *)
  let rec numeric (cs:char list) (digits:char list) =
    failwith "to be implemented (Hint : see alphanum)"

  let numeric (cs:char list) (digits:char list) =
    Debug.no_2 "numeric" 
        (add_str "cs" implode) implode 
        (pr_pair Num.string_of_num implode) 
        numeric cs digits

  let tokenof a = if List.mem a Keyword.alphas then Key a else Id a;;

  (* symbolic keyword *)
  let rec symbolic (cs:char list) (sy:string) : string * (char list) =
    match cs with
      | c::cs2 ->
            if List.mem sy Keyword.symbols 
            then (sy,cs)
            else 
              if not(List.mem c specials)
              then raise (LexErr ("Unrecognized lex symbol "^sy))
              else symbolic cs2 (sy^(Char.escaped c))
      | [] -> (sy,[]);;

  (* lexical scanner *)
  (* type: char list -> token list -> token list *)
  let rec scanning cs toks =
    match cs with
      | c::cs -> 
            if is_letter c then
              let (id,cs2) = alphanum cs [c] in
              scanning cs2 (tokenof id::toks)
            else if is_digit c then
              let (id,cs2) = numeric cs [c] in
              scanning cs2 (NumI id::toks)
            else if List.mem c specials then
              let (sy,cs2) = symbolic cs (Char.escaped c)
              in scanning cs2 (Key sy:: toks)
            else 
              (* skip spaces, line breaks, strange chars *)
              scanning cs toks
      | [] -> 
            List.rev toks

  (* type: string -> token list *)
  let scan a = scanning (explode a) []

end

module Parser =
struct
  open Lexical;;
  open Gen.Basic;;
  exception ParseErr of string;;
  (* below is a more general parser type *)
  (* 't is token type, while 'r is parse result *)
  type ('t,'r) parse = 't list -> 'r * 't list;; 
  type tokens = Lexical.token list;;
  let pr_toks = pr_list string_of_token

  (* phrase consisting of keyword *)
  (* type: string -> Lexical.token list -> string * Lexical.token list *)
  let symbol s toks =
    let exc () = (ParseErr ("Symbol "^s^" expected.")) in
    match toks with
      | (Key b)::toks -> 
            if s=b then (b,toks) 
            else raise (exc ()) 
      | _ -> raise (exc ())

  let symbol (s:string) (toks:tokens) =
    Debug.no_2 "symbol" 
        pr_id (pr_list string_of_token) 
        (fun (_,t) -> pr_toks t)
        symbol s toks

  (* type: Lexical.token list -> string * Lexical.token list *)
  let ident toks =
    match toks with
      | Id a::toks -> (a,toks)
      | _ -> raise (ParseErr ("Identifier expected"))

  let ident (toks:tokens) =
    Debug.no_1 "ident" 
        (pr_list string_of_token) 
        (pr_pair pr_id pr_toks)
        ident toks

  let symLetter toks =
    let exc c = (ParseErr ("Found "^(Char.escaped c)^". a-z expected.")) in
    match toks with
      | s::toks2 -> 
            if 'a'<= s && s <= 'z' then (s,toks2) 
            else raise (exc s) 
      | _ -> raise (exc '\n');;

  let symChar c toks =
    let exc () = (ParseErr ("Char "^(Char.escaped c)^" expected.")) in
    match toks with
      | s::toks -> 
            if s=c then (c,toks) 
            else raise (exc ()) 
      | _ -> raise (exc ())

  (* type: Lexical.token list -> Num.num * Lexical.token list *)
  let numb toks =
    match toks with
      | NumI a::toks -> (a,toks)
      | _ -> raise (ParseErr ("Identifier expected"))
  let numb (toks:tokens) =
    Debug.no_1 "numb" 
        (pr_list string_of_token) 
        (pr_pair Num.string_of_num pr_toks)
        numb toks

   (* type: (tokens -> 'a * tokens) -> ('a -> 'b) -> Lexical.token list -> 'b * tokens *)
   let (>>) (ph:('t,'a) parse) 
         (f:'a->'b)  
         (toks:'t list) : 'b*('t list) =
     let (x,toks2) = ph toks in
     (f x,toks2);;

   let (|%|) (ph1:('t,'a) parse)
         (ph2:('t,'a) parse)
         (toks:'t list) : 'a * ('t list)=
     try
       ph1 toks
     with (ParseErr _) -> ph2 toks;; 

   let (++) (ph1:('t,'a) parse)
         (ph2:('t,'b) parse)
         (toks:'t list) : ('a*'b)*('t list) =
     let (x,toks) = ph1 toks in
     let (y,toks) = ph2 toks in
     ((x,y),toks)

   let empty (toks:'t list) : 'a list * ('t list) = ([],toks)

   let rec repeat (ph:('t,'a) parse)
         (toks:'t list) : ('a list * 't list)  =
     ((ph ++ repeat ph >> (fun (a,b) -> a::b))
       |%| empty ) toks;;

   (* this combinator is to repeat a parser ph at least once *)
   let repeat1 (ph:('t,'a) parse)
         (toks:'t list) : ('a list * 't list)  =
     failwith "to be implemented"

   (* this expects ph to parse; if not exception with msg is thrown *)
   let expect (ph:('t,'a) parse) msg
         (toks:'t list) : ('a * 't list)  =
     try 
       ph toks
     with _ -> raise (ParseErr (msg));;


   (* let infixes ph prec_of apply = *)
   (*   let rec over k toks = next k (ph toks)  *)
   (*   and next k (x,toks) = *)
   (*     match toks with *)
   (*       | Key a :: toks2 -> *)
   (*             if prec_of a < k then (x, toks) *)
   (*             else next k ((over (prec_of a) >> apply a x) toks2) *)
   (*       | _ -> (x,toks)  *)
   (*   in over 0;; *)

   let reader_gen ph scanner toks =
     match (ph (scanner toks)) with
       | (x,[]) -> x
       | (x,_::_) -> raise (ParseErr "Extra chars in input")

   (* type: (Lexical.token list -> 'p * 'q list) -> string -> 'p *)
   let reader (ph:tokens->'a*tokens) 
         (toks:string) : 'a =
     reader_gen ph scan toks

end

let usage_msg = Sys.argv.(0) ^ " --dre <method-name>"
let set_source_file arg = 
  Globals.source_files := arg :: !Globals.source_files
let process_cmd_line () = 
	Arg.parse Scriptarguments.common_arguments set_source_file usage_msg;;

process_cmd_line();
Debug.read_main();

open Gen.Basic;;

open Parser;;
open Num;;

let key_let = symbol "let";;
let key_in = symbol "in";;
let key_eq = symbol "=";;
let key_plus = symbol "+";;
let key_div = symbol "/";;
let key_times = symbol "*";;
let key_minus = symbol "-";;
let key_neg = symbol "~";;
let key_open = symbol "(";;
let key_close = symbol ")";;

type exp = ENum of Num.num
           | EId of string
           | EPlus of exp * exp
           | EMinus of exp * exp
           | EDiv of exp * exp
           | ETimes of exp * exp

let rec string_of_exp e =
  match e with
    | ENum x -> Num.string_of_num x
    | EId x -> x
    | EPlus (e1,e2) -> "("^(string_of_exp e1)^"+"^(string_of_exp e2)^")"
    | EMinus (e1,e2) -> "("^(string_of_exp e1)^"-"^(string_of_exp e2)^")"
    | ETimes (e1,e2) -> "("^(string_of_exp e1)^"*"^(string_of_exp e2)^")"
    | EDiv (e1,e2) -> "("^(string_of_exp e1)^"/"^(string_of_exp e2)^")"

(*
  base ::=  Num | id | factor | "(" expr ")" | "~" term | "let" id "=" term "in" term 
  factor ::= base "*" factor | base "/" factor | base
  term ::= factor + term | factor
*)

let zero = ENum (num_of_int 0)

(* right-associative parsing *)
let rec factor tok = 
    (
     (numb >> (fun x-> ENum x))
     |%| (ident >> (fun s -> EId s))
     |%| (key_open ++ expr0 ++ key_close >> (fun ((_,e),_) -> e))
    ) tok
and term tok = 
  (
    ((factor ++ key_times) ++ term >> (fun ((e1,_),e2) -> ETimes (e1,e2)))
    |%| ((factor ++ key_div) ++ term >> (fun ((e1,_),e2) -> EDiv (e1,e2)))
    |%| (factor >> (fun e -> e))
  ) tok
and expr0 tok = 
  (
    ((term ++ key_plus) ++ expr0 >> (fun ((e1,_),e2) -> EPlus (e1,e2)))
    |%| ((term ++ key_minus) ++ expr0 >> (fun ((e1,_),e2) -> EMinus (e1,e2)))
    |%| (term >> (fun e -> e))
  ) tok
      ;;

(* LOOPING left-associative parsing *)
let rec factor tok = 
    (
     (numb >> (fun x-> ENum x))
     |%| (ident >> (fun s -> EId s))
     |%| (key_open ++ expr1 ++ key_close >> (fun ((_,e),_) -> e))
    ) tok
and term tok = 
  (
    ((term ++ key_times) ++ factor >> (fun ((e1,_),e2) -> ETimes (e1,e2)))
    |%| ((term ++ key_div) ++ factor >> (fun ((e1,_),e2) -> EDiv (e1,e2)))
    |%| (factor >> (fun e -> e))
  ) tok
and expr1 tok = 
  (
    ((expr1 ++ key_plus) ++ term >> (fun ((e1,_),e2) -> EPlus (e1,e2)))
    |%| ((expr1 ++ key_minus) ++ term >> (fun ((e1,_),e2) -> EMinus (e1,e2)))
    |%| (term >> (fun e -> e))
  ) tok
      ;;

(* left-associative parsing using repeat combinator *)
let rec factor tok = 
    (
     (numb >> (fun x-> ENum x))
     |%| (ident >> (fun s -> EId s))
     |%| (key_open ++ expr2 ++ key_close >> (fun ((_,e),_) -> e))
    ) tok

and term tok = 
  (
    ((factor ++ repeat ((key_times |%| key_div) ++ factor)) 
       >> (fun (e1,lst) -> List.fold_left (fun e (op,e2) -> if op="*" then ETimes (e,e2) else EDiv(e,e2)) e1 lst))
  ) tok

and expr2 tok = 
  (
    ((term ++ repeat ((key_plus |%| key_minus) ++ term)) 
       >> (fun (e1,lst) -> List.fold_left (fun e (op,e2) -> if op="+" then EPlus (e,e2) else EMinus(e,e2)) e1 lst))
  ) tok
      ;;

let s1 = "v+y+z";;
let s2 = "v+w*y";;
let s3 = "v/y";;
let s4 = "x+y-z+x+y";;
let s5 = "x+y*z+z-z";;
let s6 = "x+(y+z)+k";;
let s7 = "x+y+(z+a)+b";;
let s8 = "v+w*y";;
let s9 = "v*w*y";;
let s10 = "v-w-y";;
let s11 = "v-w+y";;
let s12 = "v+w-y";;
let e1 = "x*y%y/y";;
let e2 = "x + ";;
let e3 = "x + +";;
let e4 = "let let";;

let test_lex s =
  try
    let tok = Lexical.scan s in
    print_endline (s^" =lex=> "^(pr_list Lexical.string_of_token tok))
  with e -> print_endline ((Printexc.to_string e)^" encoutered for "^s);;

(* print_endline "=================";; *)
(* print_endline "Testing for Lexer";; *)
(* print_endline "=================";; *)
(* test_lex s1;; *)
(* test_lex s2;; *)
(* test_lex s3;; *)
(* test_lex s4;; *)
(* test_lex s5;; *)
(* test_lex s6;; *)
(* test_lex e1;; *)
(* test_lex e2;; *)
(* test_lex e3;; *)
(* test_lex e4;; *)

let test_parse s =
  try
    let expr = reader expr2 s in
    print_endline (s^" =parse=> "^(string_of_exp expr))
  with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

(* print_endline "==================";; *)
(* print_endline "Testing for Parser";; *)
(* print_endline "==================";; *)
(* test_parse s1;; *)
(* test_parse s2;; *)
(* test_parse s3;; *)
(* test_parse s4;; *)
(* test_parse s5;; *)
(* test_parse s6;; *)
(* test_parse s7;; *)
(* test_parse s8;; *)
(* test_parse s9;; *)
(* test_parse s10;; *)
(* test_parse s11;; *)
(* test_parse s12;; *)
(* (\* test_parse e1;; *\) *)
(* test_parse e2;; *)
(* test_parse e3;; *)
(* test_parse e4;; *)

(* test_lex "+++";; *)



module Tut9 =
struct
  let rec scan_1_i (cs:char list) : (string * char list) =
    let rec aux1 cs id =
      failwith "aux1 to be implemented" in
    match cs with
      | c::cs2 ->
            if c='a' then aux1 cs2 [c]
            else failwith "expecting a"
      | [] -> failwith "cannot match empty" 

  let rec q2_i (cs:string) : (string * string) =
    let xs = explode cs in
    let (a,rs)=scan_1_i xs in
    (a,implode rs)

(*

val regexp : string -> regexp
Compile a regular expression. The following constructs are recognized:

val search_forward : regexp -> string -> int -> int
search_forward r s start searches the string s for a substring matching the regular expression r.
The search starts at position start and proceeds towards the end of the string. Return the position of the f
irst character of the matched substring, or raise Not_found if no substring matches.

val matched_string : string -> string
matched_string s returns the substring of s that was matched by the latest Str.string_match, Str.search_forward 
or Str.search_backward. The user must make sure that the parameter s is the same string that was passed to the 
matching or searching function.

val sub : string -> int -> int -> string
String.sub s start len returns a fresh string of length len, containing the substring of s that starts at position start and has length len.
Raise Invalid_argument if start and len do not designate a valid substring of s.

*)

  let rec match_str (re:string) (cs:string) : (string * string) =
    let rexp = Str.regexp (re) in
    try
      let pos = Str.search_forward rexp cs 0 in
      (* next line is for tracing/debugging *)
      let _ = Debug.info_pprint ("found string at "^(add_str "pos" string_of_int) pos) no_pos in
      failwith "to be completed with Str.matched_string & String.sub"
    with _ -> failwith ("cannot find reg expr "^re^" in string "^cs)


  let scan_str re s =
    try
      let res = match_str re s in
      let _ = print_endline ("\n"^s^" scanned by "^re) in
      print_endline (" returns "^((pr_pair pr_id pr_id) res)) 
    with e -> 
        begin
          print_endline ((Printexc.to_string e)^" encountered");
          print_endline ("Str error matching "^s^" with "^re)
        end

  let scan_tail f s =
    try
      let res = f s in
      print_endline (s^" scanned and returns "^((pr_pair pr_id pr_id) res)) 
    with e -> 
        begin
          print_endline ((Printexc.to_string e)^" encountered");
          print_endline ("scanner error with "^s)
       end

  let q2_i_str (cs:string) =
    failwith "to be implemented"
    (* scan_str "?" cs *)

  let q2_i (cs:string) =
    scan_tail q2_i cs;;

(*
     <S> ::=  ‘(‘ <A> ‘)’
     <A> ::=  ‘[‘ <A> ‘]’
                 | ‘{‘ <A> ‘}’ <S>
                 | a | … | z
*)


let nt_S toks = 
  failwith "to be implemented with Parser.(++), Parser |%|, symChar etc"

let test_parse2 s =
  try
    let expr = reader_gen nt_S explode s in
    print_endline (s^" =parse=> "^(Char.escaped expr))
  with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

end;;

let s1 = "abdbbaa" ;;
let s2 = "abdbbaaaac" ;;
let s3 = "abdbbaaaa" ;;
let s4 = "abdbbaaa" ;;
let s5 = "abdca" ;;

print_endline "========================";;
print_endline "Using Ocaml Str module";;
print_endline "======================";;
Tut9.q2_i_str s1;; 
Tut9.q2_i_str s2;; 
Tut9.q2_i_str s3;; 
Tut9.q2_i_str s4;; 
Tut9.q2_i_str s5;; 

print_endline "========================";;
print_endline "Using Ocaml Recursive Function";;
print_endline "==============================";;
Tut9.q2_i s1;; 
Tut9.q2_i s2;; 
Tut9.q2_i s3;; 
Tut9.q2_i s4;; 
Tut9.q2_i s5;; 

let p1 = "([[a]])" ;;
let p2 = "([{b}])" ;;
let p3 = "({z})" ;;
let p4 = "({y}}" ;;
let p5 = "{1}" ;;

print_endline "========================";;
print_endline "Using Parser Combinators";;
print_endline "========================";;
Tut9.test_parse2 p1;;
Tut9.test_parse2 p2;;
Tut9.test_parse2 p3;;
Tut9.test_parse2 p4;;
Tut9.test_parse2 p5;;



