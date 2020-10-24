(*** CSI 3120 Assignment 3 ***)
(*** N/A ***)
(*** N/A ***)
(*** Sketch.sh ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(*************)
(* PROBLEM 1 *)
(*************)

(* Problem 1a: Below is a definition of the datatype for propositional
   logic from the Data Types tutorial, with connectives for
   conjunction, (the operator written /\), disjunction (the OR
   operator written \/), and logical implication (written ->).  *)
type prop = string

type form =
  | True
  | False
  | Prop of prop
  | And of form * form
  | Or of form * form
  | Imp of form * form

(* Write a function "count_connectives" that takes a "form" as input
   and counts the number of connectives.  The connectives include
   "And", "Or", and "Imp". For example, the formula below contains 5
   connectives including 3 Or, 1 And, and 1 Imp.

   ((p \/ q) /\ (False \/ p \/ r)) -> r

 *)
let count_connectives (f:form) : int = 
  let rec aux (f:form) (n:int) : int =
    match f with 
    |True -> n
    |False -> n
    |Prop x -> n
    |And(x,y) -> aux x (aux y (n+1)) 
    |Or(x,y) -> aux x (aux y (n+1)) 
    |Imp(x,y) -> aux x (aux y (n+1)) in
    aux f 0;;

(* Problem 1b: Consider the new types form' and env below *)
type form' =
  | True'
  | False'
  | Prop' of prop * bool
  | And' of form' * form'
  | Or' of form' * form'
  | Imp' of form' * form'

type env = (prop * bool) list

(* The type form' is like form, except that the constructor for
   propsitional variables includes an additional boolean argument to
   indicate the truth value of the proposition. The type env is a list
   of propositional variables and their truth values.  Write a
   function "generate_env" that takes a proposition and returns an
   "env", which simply extracts the information from the propositional
   variables in a formula and returns this information in the form of
   a list.  For example, if the input is the formula

  (((p,true) \/ (q,false)) /\ (False \/ (p,false) \/ (r,true))) -> (r,true)

   the output should be a list containing 5 pairs. Do not remove
   duplicates and don't worry about inconsistencies (the same
   propositional variable paired with both "true" and "false").  *)
let rec generate_env (f:form') : env = 
  match f with 
    |True' -> []
    |False' -> []
    |Prop' (x, y) -> [(x,y)]
    |And' (x, y) -> generate_env x @ generate_env y
    |Or' (x, y) -> generate_env x @ generate_env y
    |Imp' (x, y) -> generate_env x @ generate_env y

(* Problem 1c: Write a function "validate_env" that takes an "env" as
   input and returns tuple (bool, env).  If the input is consistent
   then return (true, original env) with the original env returned
   back in the reply.  If the input is inconsistent (i.e., there is at
   least one propositional variable that is paired with both "true"
   and "false") then return (false, duplicate envs) where you only
   return the duplicate env values

   For example, consider the formula:

   (((p,true) \/ (q,false)) /\ (False \/ (p,false) \/ (r,true))) -> (r,true)

   The env for this formula is:

   [("p",true); ("q",false); ("p",false); ("r", true); ("r" true)]

   This function should return:

   (false, [("p",true); ("p", false)]) because of the inconsistent
   occurnces of (p,true) and (p,false).

   Now consider this example:

   (((p,true) \/ (q,false)) /\ (False \/ (p,true) \/ (r,true))) -> (r,true)

   The env for this formula is:

   [("p",true); ("q",false); ("p",true); ("r", true); ("r" true)]

   This function should return:

   (true, [("p",true); ("q",false); ("p",true); ("r", true); ("r", true)])

   because the formula is consistent.

   Hint: you will likely need some helper functions.  You can choose
   to define them as local functions inside the main function, but you
   do not have to.  *)
let rec check_duplicate (x:(prop * bool)) (e:env) : (prop * bool) =
  match e with
    |[] -> x
    |h::t -> let (a, b) = x in
      let (c, d) = h in
        if a = c then h else check_duplicate x t;;

let check_same (x:(prop * bool)) (y:(prop*bool)) : bool =
  let (a,b) = x in
    let (c, d) = y in
      if b = d then
        true
      else
        false;;
        
let result (e:env) (l:env) : bool * (prop*bool) list =
  match l with
    |[] -> (true, e)
    |(_,_)::_ -> (false, l)
    
let validate_env (e:env) : bool * (prop * bool) list = 
  let r = e in
  let rec aux (e:env) (l:env) : bool * (prop * bool) list =
    match e with
      |[] -> result r l
      |x::y -> let z = check_duplicate x y in 
        if check_same x z then aux y l else aux y ([x; z] @ l)
    in aux e [];;
    
(*************)
(* PROBLEM 2 *)
(*************)

(* Below is a signature of a module for a functional version of a
   queue data structure where all elements of the queue are ints. A
   queue is a First-In-First-Out (FIFO) data structure. In a FIFO data
   structure, the first element added to the queue will be the first
   one to be removed. This is equivalent to the requirement that once
   a new element is added, all elements that were added before have to
   be removed before the new element can be removed. By reading the
   types and the comments, you will see the differences between
   stacks, as studied in class, and queues. *)
module type IntQueue =
  sig
    (* t is a queue whose elements have type int. *)
    type t

    (* The empty queue. *)
    val empty : unit -> t

    (* Whether a queue is empty. *)
    val is_empty : t -> bool

    (* [enqueue x q] is the queue [q] with [x] added to the end. *)
    val enqueue : int -> t -> t

    (* [peek q] is [Some x], where [x] is the element at the front of the queue,
       or [None] if the queue is empty. *)
    val peek : t -> int option

    (* [dequeue q] is [Some q'], where [q'] is the queue containing all the elements
       of [q] except the front of [q], or [None] if [q] is empty. *)
    val dequeue : t -> t option
end

(* Problem 2a: Implement a queue (define a module called
   ReverseListIntQueue containing a structure whose type is
   IntQueue).  Represent your queues as lists, whose elements are
   in reverse order, which means that the first element of the list is
   the last one entered into the queue, and the last element of the
   list is the front of the queue.  In other words, represent a queue
   as a list, where the list [xn; ...; x2; x1] represents the queue with
   [x1] at its front, followed by [x2], ..., followed by [xn]. You are
   of course allowed to implement helper functions inside the structure
   that do not appear in the signature above. *)
module ReverseListIntQueue : IntQueue =
  struct
    type t = int list
        
    (* The empty queue. *)
    let empty() : t = []

    (* Whether a queue is empty. *)
    let is_empty (q:t) : bool =
      match q with
        |[] -> true
        |_ -> false

    (* [enqueue x q] is the queue [q] with [x] added to the end. *)
    let enqueue (x:int) (q:t) : t = [x] @ q

    (* [peek q] is [Some x], where [x] is the element at the front of the queue,
       or [None] if the queue is empty. *)
    let rec peek (q:t) : int option = 
      match q with
        |[] -> None
        |[x] -> Some x
        |x::y -> peek y 

    (* [dequeue q] is [Some q'], where [q'] is the queue containing all the elements
       of [q] except the front of [q], or [None] if [q] is empty. *)
    let dequeue (q:t) : t option =
      let rec aux (q:t) (z:t) =
        match q with
        |[] -> None
        |[x] -> Some z
        |x::y -> aux y (z @ [x])
      in aux q []
    end

let q0 = ReverseListIntQueue.empty ()
let q1 = ReverseListIntQueue.enqueue 42 q0
let q2 = ReverseListIntQueue.enqueue 48 q1
let  i = ReverseListIntQueue.peek q2
let  j = let rest = ReverseListIntQueue.dequeue q2 in
         match rest with
         | Some q -> ReverseListIntQueue.peek q
         | None -> None
 
(* Problem 2b: Modify the IntQueue signature above so that it is
   polymorphic in the sense that it can be used to create queues of
   elements of any type, not just ints. (This should be relatively
   easy.) *)
module type Queue =
  sig
    (* t is a queue of type 'a *)
    type 'a t

    (* The empty queue. *)
    val empty : unit -> 'a t

    (* Whether a queue is empty. *)
    val is_empty : 'a t -> bool

    (* [enqueue x q] is the queue [q] with [x] added to the end. *)
    val enqueue : 'a -> 'a t -> 'a t

    (* [peek q] is [Some x], where [x] is the element at the front of the queue,
       or [None] if the queue is empty. *)
    val peek : 'a t -> 'a option

    (* [dequeue q] is [Some q'], where [q'] is the queue containing all the elements
       of [q] except the front of [q], or [None] if [q] is empty. *)
    val dequeue : 'a t -> 'a t option
end

(* Problem 2c: Modify your solution to Question 2a by
   introducing a new structure called ReverseListQueue with type
   Queue so that it can be used to create queues of elements of
   any type. (This should also be relatively easy.) *)
module ReverseListQueue : Queue =
  struct
    type 'a t = 'a list
        
    (* The empty queue. *)
    let empty() : 'a t = []

    (* Whether a queue is empty. *)
    let is_empty (q:'a t) : bool =
      match q with
        |[] -> true
        |_ -> false

    (* [enqueue x q] is the queue [q] with [x] added to the end. *)
    let enqueue (x:'a) (q:'a t) : 'a t = [x] @ q

    (* [peek q] is [Some x], where [x] is the element at the front of the queue,
       or [None] if the queue is empty. *)
    let rec peek (q:'a t) : 'a option = 
      match q with
        |[] -> None
        |[x] -> Some x
        |x::y -> peek y 

    (* [dequeue q] is [Some q'], where [q'] is the queue containing all the elements
       of [q] except the front of [q], or [None] if [q] is empty. *)
    let dequeue (q:'a t) : 'a t option =
      let rec aux (q:'a t) (z:'a t) =
        match q with
        |[] -> None
        |[x] -> Some z
        |x::y -> aux y (z @ [x])
      in aux q []
    end

let q3 = ReverseListQueue.empty ()
let q4 = ReverseListQueue.enqueue 42 q3
let q5 = ReverseListQueue.enqueue 48 q4
let i1 = ReverseListQueue.peek q5
let j1 = let rest = ReverseListQueue.dequeue q5 in
         match rest with
         | Some q -> ReverseListQueue.peek q
         | None -> None
let q6 = ReverseListQueue.empty ()
let q7 = ReverseListQueue.enqueue (3,"three") q6
let q8 = ReverseListQueue.enqueue (4,"four") q7
let i2 = ReverseListQueue.peek q8
let j2 = let rest = ReverseListQueue.dequeue q8 in
         match rest with
         | Some q -> ReverseListQueue.peek q
         | None -> None
 
(* Problem 2d: Fill in all the "..." in the modified version below
   of the IntQueue signature above so that it is a signature
   for an imperative version of an int queue called ImpIntQueue. *)
module type ImpIntQueue =
  sig
    (* t is a queue whose elements have type int. *)
    type t 

    (* Create an empty queue. *)
    val empty : unit -> t

    (* Whether a queue is empty. *)
    val is_empty : t -> bool

    (* [enqueue x q] modifies [q] by adding [x] to the end. *)
    val enqueue : int -> t -> unit

    (* [dequeue q] returns [Some x], where [x] is the element at the
       front of the queue, or [None] if the queue is empty.  It also
       modifies q by removing the front element, if there is one;
       otherwise no modifications are done.  *)
    val dequeue : t -> int option
end
 

(* Problem 2e: Modify your solution to Question 2a (using reversed
   lists as before) by introducing a new structure called
   ImpListIntQueue with type ImpIntQueue so that it
   implements an imperative queue. *)
module ImpListIntQueue : ImpIntQueue =
  struct
    type t = (int list) ref
        
    (* The empty queue. *)
    let empty() : t = ref []

    (* Whether a queue is empty. *)
    let is_empty (q:t) : bool =
      match !q with
        |[] -> true
        |_ -> false

    (* [enqueue x q] modifies [q] by adding [x] to the end. *)
    let enqueue (x:int) (q:t) : unit = q := x::(!q)
    
    (*helper function to reverse a list*)
    let rev_list (q:int list) =
      let rec rev_acc (acc: int list) = 
        function
          | [] -> acc
          | x::y -> rev_acc (x::acc) y
        in 
        rev_acc [] q;;
      
    (* [dequeue q] returns [Some x], where [x] is the element at the
       front of the queue, or [None] if the queue is empty.  It also
       modifies q by removing the front element, if there is one;
       otherwise no modifications are done.  *)
    let dequeue (q:t) : int option =
      let w = rev_list !q in
        match w with
        | [] -> None
        | x::y -> (q := rev_list y ; Some x)    
    end

let q' = ImpListIntQueue.empty ()
let _  = ImpListIntQueue.enqueue 42 q'
let _  = ImpListIntQueue.enqueue 48 q'
let  i = ImpListIntQueue.dequeue q'
let  j = ImpListIntQueue.dequeue q'
let  k = ImpListIntQueue.dequeue q'