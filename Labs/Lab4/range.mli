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
  val sadd : t -> e -> t
  (* scalar multiply range, e.g. if r is a range from 2 through 4,
     smult r 3 produces a range from 6 through 12. 
     This operation may change the size of a range. *)                        
  val smult : t -> e -> t
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
  (*returns the list*)
  val get : t -> int list
      
end