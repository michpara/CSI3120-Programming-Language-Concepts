(*** CSI 3120 Assignment 6 ***)
(*** N/A ***)
(*** N/A ***)
(*** N/A ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(*********************************************)
(* PROBLEM 1: Function-Oriented Organization *)
(*********************************************)

(* Consider the OCaml code below. *)

type fBalance = float
type fInterestRate = float
type fMonthlyPayment = float

type loan =
  | NotePayable of fBalance
  | CreditCard of fBalance * fInterestRate
  | BankLoan of fBalance * fMonthlyPayment

(* Problem 1(a) *)
(* Write a function that takes a list of loans and returns a
   list of balances.  Your function must preserve order.  For
   example, the first element of the result must be the balance of the
   first loan in the input list *)

let get_fBalances (l: loan list) : (fBalance list) = 
  let rec aux (l:loan list) (b: fBalance list) : (fBalance list) =
    match l with
    |[] -> b
    |h::t -> match h with
      |NotePayable x -> aux t (b@[x])
      |CreditCard (x, _) -> aux t (b@[x])
      |BankLoan (x, _) -> aux t (b@[x]) in
     aux l [];;

(* Problem 1(b)  *)
(* Write some test code: Create a list containing 3 loan accounts (one
   of each kind).  Set the monthly payment for the bank loan
   to 0. Apply your function from part 1(a) to your list of accounts *)
let loans = [NotePayable 6.0; CreditCard (8.0, 4.0); BankLoan (3.0, 0.0)]
let _ = get_fBalances loans

(*******************************************)
(* PROBLEM 2: Object-Oriented Organization *)
(*******************************************)

(* Your code for parts 2(a) and (b) go here *)
  
exception BalanceTooLow of fBalance

class note_payable =
  object
  val mutable fBalance : fBalance = 0.0
  method get_balance : fBalance = fBalance
  method borrow (amount: float) : unit = fBalance <- fBalance +. amount 
  method payback (amount: float) : unit = if fBalance < amount then raise (BalanceTooLow fBalance) else fBalance <- fBalance -. amount
  method to_loan : loan = NotePayable fBalance
end  

class credit_card = 
  object
  inherit note_payable as super
  val mutable fInterestRate : fInterestRate = 0.2
  method get_interest : fInterestRate = fInterestRate
  method set_interest (rate: float) : unit = fInterestRate <- rate
  method add_interest : unit = super#borrow (super#get_balance *. fInterestRate)
  method to_loan : loan = CreditCard (super#get_balance, fInterestRate)
end

class bank_loan = 
  object
  inherit note_payable as super
  val mutable fMonthlyPayment : fMonthlyPayment = 0.0
  method borrow (amount:float) : unit = super#borrow (amount); fMonthlyPayment <- fMonthlyPayment +. amount*.0.1
  method payback_monthly_amount : unit = if super#get_balance < fMonthlyPayment then raise (BalanceTooLow super#get_balance) else super#payback fMonthlyPayment
  method private get_monthly_payment : fMonthlyPayment = fMonthlyPayment
  method to_loan : loan = BankLoan (super#get_balance, fMonthlyPayment)
end

(* Problem 2(a)  *)
(* Implement an inheritance hierarchy of loans (an object-oriented
   version of the data type in Problem 1). The credit_card and
   bank_loan classes should be subclasses of the note_payable class.
   The arguments to the NotePayable, CreditCard, and BankLoan
   constructors of the loan data type should become instance
   variables.  The initial values should be 0.0 for the balance, 0.2
   (representing 20%) for the interest rate, and 0 for the monthly
   payment.  Define a method called get_balance that returns the
   balance amount.

   Use the following programming conventions.
   - Do not use abstract classes.
   - The instance variables and methods should go in the highest class
     possible in the hierarchy to maximize inheritance.  Only override
     methods when necessary.  (In this hierarchy credit cards are
     the only kind of loan with an interest rate and bank
     loans are the only kind of loan with a monthly payment.)
   - In a subclass, do not use instance variables of the super class
     directly.  For example, if the implementation of a method in
     credit_card needs to access the balance amount, then it must
     call "get_balance".

   Implement methods called "borrow" and "payback" that take one
   argument, the amount to add to (borrow) or subtract from (payback)
   the balance.  If the amount of the payback is more than the balance,
   raise an exception that takes one argument. The data returned
   when this exception is raised should be the loan balance.

   Add methods in credit_card to get and set the interest rate.
   Also add an "add_interest" method that modifies the balance by
   adding interest to the balance using the interest rate.

   In the bank_loan class, override the borrow method so that
   it also increases the monthly payment by 10% (0.1) of the borrowed
   amount.  So borrowing an additional 100.00 would add 10.00 to the
   monthly payment.  Add a "payback_monthly_amount" method that
   reduces the balance by the monthly payment amount.
   Also add a method to get the value of the monthly payment,
   but do not allow clients to set it. *)

(* Problem 2(b)  *)
(* Add a method "to_loan" to every class that transforms an
   object to the corresponding value of type loan (where
   loan is the data type defined at the beginning of this file
   just before the statement of Problem 1(a)).  (It must return an
   element of type loan where the values of the arguments are
   determined from the values of the instance variables. *)

(*********************************************)
(* PROBLEM 3: Object-Oriented "Constructors" *)
(*********************************************)
(* Problem 3(a)  *)
(* Write a function that takes an argument, creates a note_payable
   object, and then uses the argument to update the balance.  Do the
   same for credit_card and bank_loan.  The function that
   creates a credit card must take an additional argument used to
   set the interest rate. *)
let construct_note_payable (amount: float) : note_payable = 
  let n = new note_payable in
  n#borrow amount; n;;

let construct_credit_card (amount: float) (rate: float) : credit_card =
  let c = new credit_card in
  c#set_interest rate; c#borrow amount; c;;

let construct_bank_loan (amount: float): bank_loan =
  let b = new bank_loan in
  b#borrow amount; b;;

(* Problem 3(b)  *)
(* Using the same data that you used to create your solution to 1(b),
   create one object of each class, and then create a list containing
   all of them.  You may have to use the coercion operator from
   Chapter 12 of "Real World OCaml".  (See the course notes.) *)
let n = construct_note_payable 6.0;;

let c = construct_credit_card 8.0 4.0;;

let b = construct_bank_loan 3.0;;

let loans = [n; (c :> note_payable); (b :> note_payable)]

(* Problem 3(c)  *)
(* Redo Problem 1(a), writing the object-oriented version this time (a
   function that takes a list of objects of type note_payable and
   returns a list of balances.  Call your function on your list from
   Problem 3(b). *)

let get_oBalances1 (l: note_payable list) : (fBalance list) = 
  let rec aux (l: note_payable list) (b: fBalance list) : (fBalance list) =
    match l with
    |[] -> b
    |h::t -> aux t (b@[h#get_balance]) in
  aux l [];;

let _ = get_oBalances1 loans

(****************************************************)
(* PROBLEM 4: Conversion to Function-Oriented Style *)
(****************************************************)
(* Write a function that returns a list of loan balances with the
   same return values as your solution to Problem 3(c), but this time,
   your function must first take a list of note_payable objects,
   convert them to elements of type loan, and then call your
   function from Problem 1(a) *)

let get_oBalances2 (l: note_payable list) : (fBalance list) = 
  let rec aux (l: note_payable list) (b: loan list) : (loan list) =
    match l with
    |[] -> b
    |h::t -> aux t (b@[h#to_loan]) in
   get_fBalances (aux l []);;

let _ = get_oBalances2 loans