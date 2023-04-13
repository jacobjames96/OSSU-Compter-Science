(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str, strs) =
    let fun filter_str_list(strs) =
	    case strs of
		[] => []
	      | s::s'  => if same_string(s, str) then filter_str_list(s')
			  else s :: filter_str_list(s')
						   val filtered = filter_str_list(strs)
    in
	    if filtered = strs	then NONE
	    else SOME (filtered)
    end

fun get_substitutions1 (subs, s) =
    case subs of
	[] => []
     |  sb::sb' => let val x = all_except_option(s, sb)
		   in
		       case x of
			   NONE => get_substitutions1(sb', s)
			 | SOME lst => lst @ get_substitutions1(sb', s)
		   end
		       
fun get_substitutions2 (subs, s) =
    let fun aux (xs, acc) =
	    case xs of
		[] => acc
	      | x :: xs' => let val x = all_except_option(s, x)
			    in
				case x of
				    NONE => aux(xs', acc)
				 |  SOME lst => aux(xs', acc @ lst)
			    end
    in
	aux(subs, [])
    end
	
fun similar_names (subs, name) =
    let val {first=x, middle=y, last=z} = name
	val sbs = get_substitutions1 (subs, x)
	fun gen_names (alts) =
	    case alts of
		[] => []
	      | a::a' => {first=a, middle=y, last=z} :: gen_names(a')
    in
	name :: gen_names(sbs)
    end
	


								      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (c) =
    case c of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (_, _)  => Red; 

fun card_value (c) =
    case c of
	(_, Num x) => x
      | (_, Ace) => 11
      | _  => 10 

fun remove_card (cs, c, e) =
    let fun aux (cds, acc) =
	    case cds of
		[] => raise e
	      | cd :: cds' => if cd = c
			      then acc @ cds'
			      else aux (cds', acc @ [cd])
    in
	aux(cs, [])
    end

fun all_same_color (cs) =
    case cs of
	[] => true
     |  x::[] => true
     |  x::(x1::x') => (card_color(x) = card_color(x1) andalso all_same_color (x1::x')) 

fun sum_cards (cs) =
    let fun aux (cs, acc) =
	    case cs of
		[] => acc
	      | x::x' => aux(x', acc + card_value(x))
    in
	aux(cs, 0)
    end
	
fun score (cs, goal) =
    let val sum = sum_cards(cs)
	val prelim_score = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in
	if all_same_color(cs) then prelim_score div 2 else prelim_score
    end

fun officiate (cd_list, move_list, goal) =
    let fun game(cd_list, moves_left, held_cards) =
	    case (cd_list, moves_left) of
		(_, []) => score(held_cards, goal)
	      | (_, Discard x :: ml') => game(cd_list, ml', remove_card(held_cards, x, IllegalMove))
	      | ([], Draw :: ml') => score(held_cards, goal)
	      | (cd::cd', Draw :: ml') => if sum_cards(cd::held_cards) > goal
					  then score(cd::held_cards, goal)
					  else game(cd', ml', cd::held_cards)
    in
	game(cd_list, move_list, [])
	    end
