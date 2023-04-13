(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1-solution.sml";

val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1a = is_older((1, 2, 3), (1, 2, 3)) = false
val test1b = is_older((1, 2, 3), (1, 2, 4)) = true
val test1c = is_older((1, 3, 2),( 1, 2, 3)) = false

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2a = number_in_month ([(2012, 2, 28), (2013, 12, 1)], 3) = 0
val test2b = number_in_month([], 3) = 0

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3a = number_in_months([(2012, 2, 28), (2013, 12, 1)], [4]) = 0

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4a = dates_in_month([(2012, 2, 28), (2013, 12, 1)], 4) = []

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5a = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[5,6,7]) = []

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6a = get_nth (["hi", "there", "how", "are", "you"], 4) = "are"
								    
val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7a = date_to_string (2023, 12, 24) = "December 24, 2023"
					      
val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8a = number_before_reaching_sum(3, [4, 5]) = 0

val test9 = what_month 70 = 3
val test9a = what_month 2 = 1

val test10 = month_range (31, 34) = [1,2,2,2]
val test10a = month_range (35, 34) = []
					
val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11a = oldest([]) =  NONE

