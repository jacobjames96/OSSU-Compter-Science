(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw3-provided.sml";
(*
val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["A", "b", "C"] = ["A", "C"]
						
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 [] = ""
val test2b = longest_string1 ["A", "bc", "de"] = "bc"						    
						 
val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 ["A", "bc", "de"] = "de"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4b = longest_string3 [] = ""
val test4c = longest_string3 ["A", "bc", "de"] = "bc"

val test4d = longest_string4 ["A","B","C"] = "C"
val test4e = longest_string4 ["A", "bc", "de"] = "de"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5a = longest_capitalized [] = ""
val test5b = longest_capitalized ["a", "b", "c"] = ""
val test5c = longest_capitalized ["Ab", "Cd"] = "Ab"
						     

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x > 0 then SOME [x] else NONE) [2, 3, 4, 5, 6, 7] = SOME [2, 3, 4, 5, 6, 7]
val test8b = all_answers (fn x => if x > 0 then SOME [x, 1] else NONE) [2, 3, 4] = SOME [2, 1, 3, 1, 4, 1]
											      
val test9a = count_wildcards Wildcard = 1
val test9aa = count_wildcards UnitP = 0
val test9ab = count_wildcards (ConstructorP("Test", UnitP)) = 0
val test9ac = count_wildcards (ConstructorP("Test", Wildcard)) = 1
val test9ad = count_wildcards (TupleP [Wildcard, Wildcard, Wildcard]) = 3
					  

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9ba = count_wild_and_variable_lengths (Variable("abcd")) = 4
val test9bb = count_wild_and_variable_lengths (TupleP [Wildcard, Variable("a")]) = 2
val test9bc = count_wild_and_variable_lengths (UnitP) = 0
								   
*)
val test9c = count_some_var ("x", Variable("x")) = 1
val test9ca = count_some_var ("x", Variable("y")) = 0
val test9cb = count_some_var ("x", ConstructorP("x", Wildcard)) = 0

val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Variable "x"]) = false

val test11 = match (Const(1), UnitP) = NONE
val test11a = match (Const(1), Wildcard) = SOME []
val test11b = match (Const(1), ConstP(1)) = SOME []

						

val test12 = first_match Unit [UnitP] = SOME []

