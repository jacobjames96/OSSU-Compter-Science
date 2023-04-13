(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 xs =
    foldl (fn (x, acc) => if String.size x > String.size acc then x else acc) "" xs

fun longest_string2 xs =
    foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc) "" xs

fun longest_string_helper f =
    foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) ""

val longest_string3 = longest_string_helper (fn (x, acc) => x > acc)

val longest_string4 = longest_string_helper (fn (x, acc) => x >= acc)

val longest_capitalized = longest_string1 o only_capitals
					   
fun rev_string s =
    (String.implode o List.rev o String.explode) s

fun first_answer f lst =
    case lst of
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs'
		    | SOME v => v

fun all_answers f lst =
    let fun aux (lsta, acc) =
	    case (lsta, acc) of
		([], a)  => a
		  | (x::xs', SOME v) => case f x of
					  NONE => NONE
					 | SOME x => aux(xs', SOME (v @ x)) 
    in
	aux(lst, SOME [])
    end

fun count_wildcards p =
    g (fn x => 1) (fn y => 0) p
    
fun count_wild_and_variable_lengths p =
    g (fn x => 1) (fn y => String.size y) p

fun count_some_var (s, p) =
    g (fn x => 0) (fn y => if y = s then 1 else 0) p

fun check_pat (p) =
    let fun var_names (patt) =
	    case patt of
		Variable x        => [x]
	      | TupleP ps         => List.foldl (fn (x, acc) => var_names x @ acc ) [] ps
	      | ConstructorP(_,p) => var_names p
	      | _                 => []

	fun repeats (los) (acc) =
	    case los of
		[] => true
	      | s::los' => if List.exists (fn elem => s = elem) acc then false
			   else repeats los' (s::acc)
    in
	repeats (var_names p) []
    end

fun match (vp: valu * pattern) : (string * valu) list option =
    case vp of
	(_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | ((Const i), (ConstP j)) => if i = j then SOME [] else NONE
      | ((Tuple vs), (TupleP ps)) => if List.length vs = List.length ps then all_answers match (ListPair.zip(vs, ps))
				     else NONE
					 
      | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2 then match (v, p)
						     else NONE
      | _  => NONE 
	
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle NoAnswer => NONE
