(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str, strings) = 
    let fun iter (strings, acc) = 
            case strings of 
                []             => NONE
              | head::strings' => if same_string(head, str)
                                  then SOME (acc @ strings')
                                  else iter(strings', head::acc)
    in iter(strings, [])
    end

fun get_substitutions1(substitutions, s) =
     case substitutions of
         [] => []
       | x::xs => case all_except_option (s, x) of
                          NONE => get_substitutions1(xs, s) 
                         | SOME strs => strs @ get_substitutions1(xs, s) 
                          
fun get_substitutions2 (substitutions, s) =
      let fun aux(subs, acc) =
                   case subs of
                        [] => acc
                      | x::xs =>  case all_except_option (s, x) of
                                                 NONE => aux(xs, acc) 
                                               | SOME strs => aux(xs, acc @ strs) 
       in aux(substitutions, [])
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
