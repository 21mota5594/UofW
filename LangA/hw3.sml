(**** you can put all your code here ****)
val only_capitals = List.filter (fn x => (Char.isUpper o String.sub) (x, 0))

val longest_string1 = List.foldl (fn (x, acc) => if String.size x > String.size acc then x else acc) ""

val longest_string2 = List.foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc) ""

fun longest_string_helper f = List.foldl (fn (x, acc) => if f (String.size x, String.size acc) then x else acc) ""

val longest_string3 = longest_string_helper(fn (x, acc) => x > acc)

val longest_string4 = longest_string_helper(fn (x, acc) => x >= acc)

val longest_capitalized = longest_string1 o only_capitals
						
val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =  case xs of
				  [] => raise NoAnswer
				| x::xs' => case f x of
						 SOME v => v
					       | NONE => first_answer f xs'

fun all_answers f =
       let fun answers (xs, acc) =
		 case xs of
			 [] => SOME acc
		       | x::xs' => case f x of
				       SOME y => answers(xs', acc @ y)
				     | NONE => NONE
       in fn xs => answers (xs, [])
       end

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

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x)

fun count_some_var (str, patt) = g (fn _ => 1) (fn x => if x = str then 1 else 0) patt
	
fun variables p =
    case p of
	Variable x => [x]
      | TupleP ps => List.foldl (fn (y, acc) => (variables y) @ acc) [] ps
      | ConstructorP(_,p) => variables p
      | _ => []  

fun check_repeats xs =
    case xs of
	[] => true
      | x::xs' => if List.exists (fn n => x = n) xs' then false else check_repeats xs'
		      
val check_pat = check_repeats o variables

fun match (v, p) =
    case (v, p) of
	(_,Wildcard) => SOME []
      | (v, Variable s) => SOME [(v,s)]
      | (Unit,UnitP) => SOME []
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (Constructor(s2,v'),ConstructorP(s1,p')) => if s1 = s2 then match(v', p') else NONE
      | (Tuple vs, TupleP ps) => if length vs = length ps then all_answers match (ListPair.zip(vs,ps))
				 else NONE
      | _ => NONE
				     
    
fun first_match a b =
    case first_answer (fn x => match(a,x)) b of
	y => SOME y
     handle NoAnswer => NONE
