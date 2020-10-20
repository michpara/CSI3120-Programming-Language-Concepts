(* A condensed version of the signature in range.mli.  Your first step is to study the contents of range.mli. *)
module type RANGE =
sig
  type t
  type e
  val singleton : e -> t
  val range : e -> e -> t
  val sadd : t -> e -> t
  val smult : t -> e -> t
  val bridge : t -> t -> t
  val size : t -> int
  val contains : t -> e -> bool
  val rless : t -> t -> bool option
end

(* An implementation of the RANGE datatype with int as range type and
   pairs representing a range *)
  module LoHiPairRange : RANGE with type e = int =
struct
  type e = int
  type t = e * e
  let singleton (i:e) : t = (i,i)
  let range (i:e) (j:e) : t = ((min i j), (max i j))
  let sadd (x:t) (i:e) : t = let (lo,hi) = x in (lo+i,hi+i)
  let smult (x:t) (i:e) : t =
    let (lo, hi) = x in
    if i >= 0 then (lo*i,hi*i)
    else (hi*i,lo*i)
  let bridge (x:t) (y:t) : t =
    let (lx, hx) = x in
    let (ly, hy) = y in
    ((min lx ly), (max hx hy))
  let size (x:t) : int =
    let (lo,hi) = x in
    hi - lo - (-1)
  let contains (x:t) (i:e) : bool =
    let (lo,hi) = x in
    (lo <= i) && (i <= hi)
  let rless (x:t) (y:t) : bool option =
    let (lx, hx) = x in
    let (ly, hy) = y in
    if hx < ly then Some true
    else if hy < lx then Some false
    else None
end

(* Exercise 1: Complete the new implementation of RANGE in the
     ListRange module below.  The part that is already implemented
     should give you enough information to implement the rest.  Add
     some test code to test your implementation. *)
    
(* An implementation of the RANGE datatype with int as range type and
   lists representing a range *)
module ListRange : RANGE with type e = int =
struct
  type e = int
  type t = e list

  (* auxiliary functions *)
  let minmax (l:t) : (e*e) option =
      let rec max (t:t) (e:e) : e =
          match t with
          | [] -> e
          | h::r -> max r h
      in
      match l with
      | [] -> None
      | h::r -> Some (h, (max r h))
  let rec build (i:e) (j:e) : e list =
    if i = j then [j]
    else i :: build (i+1) j
  
  let singleton (i:e) : t = [i]
  let range (i:e) (j:e) : t = build (min i j) (max i j)
  (* TODO Exercise 1: Replace all the code below with correct implementations of the operations. *)
  let sadd (x:t) (i:e) : t = 
    let Some(a, b) = minmax(x) in build (a+i) (b+i)
  let smult (x:t) (i:e) : t =     
      if i >= 0 then 
        let Some (a,b) = minmax(x) in build (a*i) (b*i)
      else 
        let Some (a, b) = minmax(x) in build (b*i) (a*i)
  let bridge (x:t) (y:t) : t = 
	  let Some (a, b) = minmax(x) in
	    let Some (c, d) = minmax(y) in
	      if a < c then
	        if b < d then
	          build a d
	        else
	          x
	      else
	        if b < d then
	          y
	        else
	          build c b;;
  let size (x:t) : int = 
      let rec aux (y:t) (n:int) = match y with
      | [] -> n
      | h::t -> aux t (n+1) in
    aux x 0;;
  let contains (x:t) (i:e) : bool =     
  	let rec aux (y:t) = match y with
      | [] -> false
      | h::t -> if i = h then true else aux t in
    aux x;;
  let rless (x:t) (y:t) : bool option = 
    let Some (lx, hx) = minmax(x) in
    let Some (ly, hy) = minmax(y) in
    if hx < ly then Some true
    else if hy < lx then Some false
    else None
end

(* TODO Exercise 1: Add some test code to test your new implementation. *)
let l1 = ListRange.singleton 5 (*Create a one element ListRange*)
let _ = (ListRange.size l1 = 1) (*Make sure only one element in list range*)

let l2 = ListRange.range (-4) 6 (*Create a ListRange from -4 to 6*)
let _ = (ListRange.size l2 = 11) (*Make sure the size is 11*)

let a1 = ListRange.sadd l2 2 (*Increment range by 2*)

let m1 = ListRange.smult l2 2 (*multiply range by 2*)
let m2 = ListRange.smult l2 (-3) (*multiply range by -3 (should be reversed)*)

let l3 = ListRange.range 1 10 (*Create a ListRange from 1 to 10*)
let l4 = ListRange.range 0 11 (*Create a ListRange from 0 to 11*)
let l5 = ListRange.range 2 9 (*Create a ListRange from 2 to 9*)

let b1 = ListRange.bridge l2 l3 (*Create a new range from -4 to 10 *)
let b2 = ListRange.bridge l3 l2 (*Create a new range from -4 to 10 (again)*)
let b3 = ListRange.bridge l3 l4 (*Should return l4*)
let b4 = ListRange.bridge l5 l3 (*Should return l3*)

let _ = ListRange.contains l3 2 (*Check if l3 contains 2 (true)*)
let _ = ListRange.contains l3 11 (*Check if l3 contains 11 (false)*)

let l6 = ListRange.range (-4) 1 (*Create a ListRange -4 to 1*)
let _ = ListRange.rless l6 l5 (*Check if l6 is smaller than l5 (true)*)
let _ = ListRange.rless l5 l6 (*Check if l5 is smaller than l6 (false)*)
let _ = ListRange.rless l2 l3 (*Check if l2 is smaller than l3 (none because they overlap)*)

(* Exercise 2: Design an imperative version of RANGE.  Do so by
   copying range.mli here and changing the types as necessary.  And
   then copy the implementation of LoHiPairRange and modify the code
   as necessary.  All the operations should remain the same as in the
   functional version.  The singleton and range operations should each
   create a new range.  The sadd and smult operations should modify
   existing ranges. Consider the design choices and design your own
   version of bridge. *)
module type RANGE =
sig
  (* types *)
  (* RANGE type *)
  type t 
  (* element type *)
  type e
    
  (* constructors *)
  (* construct a one-item range *)
  val singleton : e -> t
  (* construct a range with two endpoints, inclusive *)
  val range : e -> e -> t

  (* modifiers *)
  (* scalar add range, e.g. if r is a range from -4 through 6, 
     sadd r 1 produces a range from  -3 through 7. 
     This operation does not change the size of a range. *)
  val sadd : t -> e -> unit
  (* scalar multiply range, e.g. if r is a range from 2 through 4,
     smult r 3 produces a range from 6 through 12. 
     This operation may change the size of a range. *)                        
  val smult : t -> e -> unit
  (* create a new range that spans both given ranges, e.g.
     if given a range from -4 through 6 and a range from 10 through 12, 
     produces a range from -4 through 12. *)
  val bridge : t -> t -> t 

  (* observers *)
  (* how many elements are in the range? *)
  val size : t -> int
  (* does t contain e? *)
  val contains : t -> e -> bool
  (* is an arbitrary element of the first range 
      less than an arbitrary element of the second range?
     if the ranges overlap, return None, because 
      answers differ depending on the element chosen
     otherwise return whether the first range's max < second range's min
   *)
  val rless : t -> t -> bool option
end

module type RANGE =
sig
  type t 
  type e
  val singleton : e -> t
  val range : e -> e -> t 
  val sadd : t-> e -> unit
  val smult : t -> e -> unit
  val bridge : t -> t -> t 
  val size : t -> int
  val contains : t -> e -> bool
  val rless : t -> t -> bool option
end


  module LoHiPairRange : RANGE with type e = int =
struct
  type e = int
  type t = (e * e) ref
  let singleton (i:e) : t = ref (i,i)
  let range (i:e) (j:e) : t = ref ((min i j), (max i j))
  let sadd (x:t) (i:e) : unit = let (lo,hi) = !x in x := (lo+i, hi+i)
  let smult (x:t) (i:e) : unit =
    let (lo, hi) = !x in
    if i >= 0 then x := (lo*i,hi*i)
    else x := (hi*i,lo*i)
  let bridge (x:t) (y:t) : t =
    let (lx, hx) = !x in
    let (ly, hy) = !y in
    ref((min lx ly), (max hx hy))
  let size (x:t) : int =
    let (lo,hi) = !x in
    hi - lo - (-1)
  let contains (x:t) (i:e) : bool =
    let (lo,hi) = !x in
    (lo <= i) && (i <= hi)
  let rless (x:t) (y:t) : bool option =
    let (lx, hx) = !x in
    let (ly, hy) = !y in
    if hx < ly then Some true
    else if hy < lx then Some false
    else None
end