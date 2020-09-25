(* sum the numbers from 0 to n 

   sumTo 0 = 0
   sumTo 3 = 6
   sumTo (-1) = 0
*)
let rec sumTo (n:int) : int = 
  if n <= 0 then
    0
  else
    n + sumTo (n-1)

let _ =
  print_int (sumTo 8);
  print_newline()
