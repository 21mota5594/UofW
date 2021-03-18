(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, sl) =
  case sl of
    [] => NONE
    | x::xs => case same_string(str, x) of
                 true => SOME(xs)
                 | false => case all_except_option(str, xs) of
                              NONE => NONE
                              | SOME y => SOME(x::y)

fun get_substitutions1 (subs, s) =
  case subs of
    [] => []
    | (x :: xs) => case all_except_option(s, x) of 
                     NONE => get_substitutions1(xs, s)
                     | SOME(ys) => ys @ get_substitutions1(xs, s) 

fun get_substitutions2 (slist, s) =
    let fun aux (slist, acc) =
	    case slist of
		[] => acc
	      | x::xs => case all_except_option(s, x) of
			     NONE => aux(xs, acc)
			  | SOME ys  => aux(xs, acc @ ys) 
    in
	aux(slist, [])
    end


	    
fun similar_names (lists, {first, middle, last}) =
    let fun add_records (slist, {first, middle, last}) =
	let fun aux (names, acc) =
		case names of
		    [] => acc
		  | x::xs => aux(xs, [{first=x, middle=middle, last=last}] @ acc)
	in  aux(slist, [])
	end
    in add_records(get_substitutions1(lists, first), {first=first, middle=middle, last=last})
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
fun card_color (suit, rank) =
    case suit of
	Clubs => Black
      | Diamonds => Red
      | Hearts => Red 
      | Spades  => Black 
						    
						    
fun card_value (suit, rank) =
    case rank of
	Num i => i
      | Ace => 11
      | _  => 10 

fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x::xs => case c = x of
		     true => xs
		   | false => case remove_card(xs, c, e) of
				  [] => [x]
				| y::ys => x::y::ys

fun all_same_color (cards) =
    case cards of
	[] => true
     | last::[] => true
     | head::(neck::rest) => card_color(head) = card_color(neck) andalso all_same_color(rest)

fun sum_cards (cards) =
    let fun aux (cards, acc) =
	    case cards of
		[] => acc 
	      | card::others => aux(others, card_value(card) + acc)
    in
	aux(cards, 0)
    end
	
fun score (cards, goal) =
    let fun check(cards) =
	    case all_same_color(cards) of
		true => 2
	      | false => 1
    in
    case sum_cards(cards) > goal of
	true => (sum_cards(cards) - goal) * 3 div check(cards) 
     | false => (goal - sum_cards(cards)) 
    end
	
fun officiate (cards, moves, goal) =
    let fun movesfun (cards, held, moves) =
	    case moves of
		[] => score(held, goal)
	      | x::xs => case x of
			     Discard card => movesfun(cards, remove_card(held, card, IllegalMove), xs) 
			   | Draw => case cards of
				[] => score(held, goal)
				| y::ys => case (card_value(y) + sum_cards(held) > goal) of
					      true => score(y::held, goal)
					    | false => movesfun(ys, y::held, xs)
    in
	movesfun(cards, [], moves)
    end

fun score_challenge (cards, goal) =
    let fun aux (cards, acc) =
	    case cards of
		[] => 0
	      | (suit, rank)::xs => case rank of
			     Ace => 1
			   | _ =>  card_value(suit, rank) + aux(xs, goal)
    in
	aux(cards, 0)
    end
	
fun officiate_challenge (cards, moves, goal) =
    let fun movesfun (cards, held, moves) =
	    case moves of
		[] => score_challenge(held, goal)
	      | x::xs => case x of
			     Discard card => movesfun(cards, remove_card(held, card, IllegalMove), xs) 
			   | Draw => case cards of
				[] => score_challenge(held, goal)
				| y::ys => case (card_value(y) + sum_cards(held) > goal) of
					      true => score_challenge(y::held, goal)
					    | false => movesfun(ys, y::held, xs)
    in
	movesfun(cards, [], moves)
    end
	
