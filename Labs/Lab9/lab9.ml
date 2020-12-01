(* Pipelines in OCaml *)
(* In this lab, you will use pipelining to calculate and display final
   marks for students in a course. *)

(* The type "marks" is a tuple of 6 floating point numbers.  The first
   3 are marks for 3 assignments.  The next 2 are marks for two term
   tests.  The last one is the mark for the final exam. *)
type marks = float * float * float * float * float * float

(* The type "mark_triple" is a tuple of 3 floating point numbers.  The
   first is the assignment mark for the course.  The second is the
   term test mark for the course.  The third is the mark for the final
   exam. *)
type mark_triple = float * float * float

(* The type "final_grade" is the student's final mark represented as a
   percentage. *)
type final_grade = float

(* The type "letter_grade" represents the student's final mark as it
   will appear on their transcript (A+, A, ...) *)
type letter_grade = string

(* The calculations will involve a student_id and 3 kinds of tuples
   representing 3 different forms of student records. *)
type student_id = int
type st_record1 = student_id * marks
type st_record2 = student_id * mark_triple
type st_record3 = student_id * final_grade * letter_grade

(* Assignment 1 is worth a total of 60 marks, Assignment 2 is worth
   75, and Assignment 3 is worth 40.  Each term test is worth 50 and
   the final exam is marked out of 100. *)
let total_a1 : float = 60.
let total_a2 : float = 75.
let total_a3 : float = 40.
let total_t1 : float = 50.
let total_t2 : float = 50.
let total_exam : float = 100.
let perfect_score : marks = (total_a1,total_a2,total_a3,total_t1,total_t2,total_exam)

(* The marking scheme for the course is that the assignments are worth
   33%, the term tests are worth 33% and the final exam is worth 34%. *)
let assign_percent1 = 33.
let test_percent1 = 33.
let exam_percent1 = 34.

(* The following function may be useful for tranforming a mark to a
   percentage. *)
let out_of_100 (max_marks:float) (actual_marks:float) : float =
  (actual_marks *. 100.) /. max_marks

(* QUESTION 1.  Define an OCaml function that takes a st_record1, and
   uses a pipeline to do the following operations:

(a) First modify the exam component of each student record.  In the
   input record, each student's mark is out of 100 points.  The
   professor has decided to mark it out of 95 points.  So, if a
   student got 95 on the exam, their mark will be converted to 100.
   If the student got 96, their mark will be converted to 101.05.  If
   the student got 94, their mark will be converted to 98.95, etc.*)

let convert_exam_mark (s: st_record1) : (st_record1) =
  match s with
  (s, (a1,a2,a3,m1,m2,e)) ->
  let e_new = out_of_100 95.0 e in
  (s, (a1, a2, a3, m1, m2, e_new))

(*(b) Next, transform each st_record1 to a st_record2 by calculating
   the total number of marks the student got on the assignment portion
   of the course, the term test component, and the final exam
   component.*)
 
let transform (s: st_record1) : (st_record2) = 
  match s with
  (s, (a1, a2, a3, m1, m2, e)) ->
  let a = a1 +. a2 +. a3 in
  let m = m1 +. m2 in
  (s, (a, m, e))

(*(c) Next, modify each of the 3 mark components of st_record2 by
   transforming them to a percentage.*)
   
let transform_to_percentage (s:st_record2) : st_record2 =
  match s with
  (s, (a, m, e)) ->
  let a_new = out_of_100 (total_a1 +. total_a2 +. total_a3) a in
  let m_new = out_of_100 (total_t1 +. total_t2) m in
  (s, (a_new, m_new, e))
    
(*(d) Next, modify each one again by transforming it to the appropriate
   portion allowed by the marking scheme.  For example, if the student
   got 100% on the assignment portion of the course, the 100 in the
   assignment position of the tuple of type student_record2 should be
   replaced by 33, because the assignment portion of the course is
   worth 33% of the total mark.  If the student got 50% on the
   assignment portion, this value should be replaced by half of 33,
   which is 16.5, etc.*)
let modify_portion (s: st_record2) : st_record2 =
  match s with
  (s, (a, m, e)) ->
  let div = a/.100.0 in
  let a_new = div*.assign_percent1 in
  let div = m/.100.0 in
  let m_new = div*.test_percent1 in
  let div = e/.100.0 in
  let e_new = div*.exam_percent1 in
  (s, (a_new, m_new, e_new))

(*(e) Transform the st_record2 that is obtained from step (d) to a
   st_record3, by summing the 3 mark components of the st_record2 and
   using the result to calculate the letter grade using the University
   of Ottawa grading scheme. *)
   
let transform_final (s:st_record2) : st_record3 =
  match s with
  (s, (a, m, e)) ->
  let grade = a +. m +. e in
  let letter = 
  if grade < 40.0 then "F"
  else if grade < 50.0 then "E"
  else if grade < 55.0 then "D"
  else if grade < 60.0 then "D+"
  else if grade < 65.0 then "C"
  else if grade < 70.0 then "C+"
  else if grade < 75.0 then "B"
  else if grade < 80.0 then "B+"
  else if grade < 85.0 then "A-"
  else if grade < 90.0 then "A"
  else "A+" in
  (s, grade, letter)

(* QUESTION 2 *)
(* Define a version of the "display" function on page 18 of the course
   notes on pipelines that works with the data in this lab.  The type
   of the input argument to your version of "display" will be
   "st_record1 list", and you will use your solution to Question 1
   instead of "compute_score".  You will also need to define a new
   version of "compare_score" and "stringify". *)
let compare_score (_,score1,_) (_,score2,_) =
   if score1 < score2 then 1
   else if score1 > score2 then -1
   else 0

let stringify (id, score, letter_grade) =
   string_of_int id ^ ": " ^ string_of_float score ^ ", " ^ letter_grade

let display (ss : st_record1 list) : unit =
ss |> List.map convert_exam_mark
   |> List.map transform
   |> List.map transform_to_percentage
   |> List.map modify_portion
   |> List.map transform_final
   |> List.sort compare_score
   |> List.map stringify
   |> List.iter print_endline