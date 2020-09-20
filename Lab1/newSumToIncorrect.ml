(* sum the numbers from 0 to n 
   precondition: n must be a natural number

   sumTo 0 ==> 0
   sumTo 3 ==> 6
*)
let rec sumTo (n:int) : int =

  match n with
    0 -> 0
  | n -> n + sumTo (n-1)
;;

let _ =
  print_int (sumTo 8);
  print_newline()
