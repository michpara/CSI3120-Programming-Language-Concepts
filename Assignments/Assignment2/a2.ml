(*** CSI 3120 Assignment 2 ***)
(*** N/A ***)
(*** N/A ***)
(*** Sketch.sh ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(*************)
(* PROBLEM 1 *)
(*************)

(* For each part of problem 1, explain in the given string why the code
   will not typecheck, then follow the instructions for that part to
   change the code so that it typechecks while keeping the same values
   as intended by the erroneous code. Once you have done so, uncomment
   the fixed expression for submission.
*)

(* Problem 1a - Give your explanation in exp1a and then fix the
   right-hand-side of the assignment to match the listed type.
   (Do not change the left-hand-side.)
*)

let exp1a : string = "prob1a was returning a list with a few variables with different types [string, int, char].
However, the return value of prob1a should be a list of tuples that contain a string, integer and char (string, integer, char)."

let prob1a : (string * int * char) list = [("7", 8, '9')];;

(* Problem 1b - Give your explanation in exp1b and then fix the type
   of variable prob1b to match the type of the expression on the
   right-hand-side of the assignment. (Do not change the
   right-hand-side.)
 *)

let exp1b : string = "prob1b was returning a tuple with 2 lists, one of type string and one of type int.
However, the return value of prob1b should be a list of tuples containing a strng and integer [(string, integer)."

let prob1b : (string * int) list = [("apples", 3); ("bananas", 2); ("carrots",1)];;

(* Problem 1c - Give your explanation in exp1c and then fix the
   right-hand-side of the expression to match the variable prob1c's
   listed type.  (Do not change the left-hand-side.)
 *)

let exp1c : string = "prob1c was returning a list of lists containing lists of strings [[string]; [string]; :: [string]; [string]].
However, the return value of prob1c should be a list of lists containing strings."

let prob1c : string list list = ["2"; "b"] :: ["or"; "not"; "2b"] :: ["that is"; "the"] :: ["question"] :: []

(*************)
(* PROBLEM 2 *)
(*************)

(* Fill in expressions to satisfy the following types:
 *
 * NOTE: for option, list, and function types, you must
 * provide a nontrivial answer. For a list that means a
 * non-empty one, for an option type that means a Some
 * construction, and for a function, that means using
 * its arguments to generate the return value.
 * example problems:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * incorrect answers:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * possible correct answers:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int =
 *         String.length  ((string_of_int x) ^ (string_of_int y))
 *)

(* Problem 2a *)

let prob2a : (int * ((string * float) option list)) list = [(7, [Some ("hello", 3.14)])]

(* Problem 2b *)
(* a pet is a (name, animal_type, age option) tuple *)

type pet = string * string * int option

let prob2b : string * pet list option = ("Michelle", Some [("Munchkin", "Cat", Some 5)])

(* Problem 2c *)
(* Fill in a valid function call to f to make prob2c typecheck *)

let prob2c =
  let rec f arg =
    match arg with
    | (a, b) :: xs -> if a then (b ^ (f xs)) else f xs
    | _ -> ""
  in f [(true, "Hello"); (false, "Not"); (true, " World")]

(*************)
(* PROBLEM 3 *)
(*************)

(* Problem 3a.  You have been asked to write a text filter,
   where you want to find all search characters in your text
   if they appear the right order.

   Write a function text_filter that takes two lists of characters
   and checks to see if all the characters in the first list are
   included in the second list AND in the same order, BUT possibly
   with other characters in between.  For example

   text_filter ['a';'m';'z'] ['1';'a';'2';'m';'3';'z'] = true
   text_filter ['a';'m';'z'] ['1';'a';'3';'z'] = false
   text_filter ['a';'m';'z'] ['1';'z';'2';'m';'3';'a'] = false *)

let rec text_filter (xs:char list) (ys:char list) : bool = match xs with
  	| [] -> true
  	| h :: t -> if List.mem h ys then 
  		let rec f (xs: char list) (ys: char list) : bool = match ys with
        	|[] -> false
     		|h2 :: t2 -> 
     			if List.mem h2 xs then 
         			if h2 != h then false 
         			else text_filter t t2 
       			else f xs t2 
     	in f xs ys
  	else false;;

(* Problem 3b. Rewrite the function above so that is is polymorphic,
   i.e., it should work on lists whose elements are any types.  Give
   at least one test case (call your function at least once) with a
   type that is different from chars. *)
let rec text_filter (xs:'a list) (ys:'a list) : bool = match xs with
  	| [] -> true
  	| h :: t -> if List.mem h ys then 
  		let rec f (xs:'a list) (ys:'a list) : bool = match ys with
        	|[] -> false
     		|h2 :: t2 -> 
     			if List.mem h2 xs then 
         			if h2 != h then false 
         			else text_filter t t2 
       			else f xs t2 
     	in f xs ys
  	else false;;
  	
(* test cases *)
text_filter [1;2;3] [1;203;2;4;3;9] = true;;
text_filter [1;2;3] [1;203;3;9] = false;;
text_filter [1;2;3] [3;9;2;4;1;203] = false;;

(*************)
(* PROBLEM 4 *)
(*************)

(* Write a function (int_to_whole) that converts an integer
   into a whole number if one exists
   (a whole number is 1, 2, 3, ...).
   Use an option type because not all integer inputs can
   be converted. *)

type whole = One | Succ of whole

let int_to_whole (x: int) : whole option = 
  if x < 1 then
    None
  else
    let rec f (y: int) : whole =
      match y with 
        | 1 -> One
        | y -> Succ (f (y-1)) in
    Some (f x);;
