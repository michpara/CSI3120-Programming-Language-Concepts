(* 
  Exceptions and Continuations
 *)

(* QUESTION 1. Exceptions and Recursion *)
(* Consider the following kinds of execution steps, which you will use
   to trace some code:
   - function call (with argument)
   - function return (with return value)
   - raise an exception
   - handle an exception
   - pop activation record of function off the stack without returning
     control to the function

   The last one happens when a function f calls a function g (which
   could be a recursive call to f), and g raises an exception that f
   does not handle.  In this case the activation record of f is popped
   off the stack without returning control to f.

   Now consider the following OCaml function f1 that uses an exception
   called Odd.  *)

exception Odd
let rec f1 (x:int) =
  match x with
  | 0 -> 1
  | 1 -> raise Odd
  | 3 -> f1 (3-2)
  | _ -> try f1 (x-2) with | Odd -> (-x)

(* Give the sequence of steps for the function call (f1 11).  The
   first one is "call (f1 11)" and the second one is "call (f1 9)".
   Continue from here. *)

(* f1(11), f1(9), f1(7), f1(5), f1(3), f1(1), Odd*)

(* QUESTION 2. Exceptions and Memory Management *)
(* The following two versions of the "closest" function take an
   integer x and a tree t and return the integer leaf value fron t
   that is closest in absolute value to x.  The first is a
   straightforward recursive function and the second uses
   exceptions. Both are applied to the same simple example tree.

   Draw the activation stacks for the execution of each of these
   programs. In your activation records, include the access links,
   parameters, local variables (including handlers), and return-result
   addresses. The value for the return-result address of the first
   function call can be blank, but the value of the return-result
   address for recursive calls must point to the right place. You can
   leave out control links and return addresses.  Include intermediate
   results for (abs (x-lf)) and (abs (x-rt)) in the calls to the
   "closest" function.  Use closures for functions.

   If any activation records need to be popped off the stack, do not
   erase them, but instead mark them as popped and continue below
   them.

   Explain why the second is sometimes more efficient than the
   first. *)

type 'a tree =
  | Leaf of 'a
  | Nd of 'a tree * 'a tree

let _ =
  let tree1 = Nd (Leaf 1,Leaf 2) in
  let rec closest (x:int) (t:int tree) =
    match t with
    | Leaf y -> y
    | Nd (y,z) ->
       let lf = closest x y in
       let rt = closest x z in
       if abs (x-lf) < abs (x-rt) then lf else rt
  in closest 1 tree1

exception Found of int
let _ =
  let tree1 = Nd (Leaf 1,Leaf 2) in
  let rec closest (x:int) (t:int tree) =
    match t with
    | Leaf y -> if x=y then raise (Found x) else y
    | Nd (y,z) ->
       let lf = closest x y in
       let rt = closest x z in
       if abs (x-lf) < abs (x-rt) then lf else rt
  in try closest 1 tree1 with | Found n -> n


(* QUESTION 3. Continuations and Exceptions *)
(* Consider the following OCaml functions, which use continuations for
   both normal computation and exceptions.  Write a new version of
   this code that does the same computation, but does not use any
   continuations and uses an exception instead of a continuation for
   the error condition.  *)

(*Continuations*)
let f (x:int) (normal_cont:int->int) (exception_cont:unit -> int) =
  if x < 0
  then exception_cont()
  else normal_cont (x/2)
let g (y:int) = f y (fun z -> 1+z) (fun () -> 0)

(*Exceptions*)
exception Div
let f (x:int) =
  if (x < 0) then raise Div
  else (x/2)+1;;
  
let g (y:int) = try f y with |Div -> 0;;