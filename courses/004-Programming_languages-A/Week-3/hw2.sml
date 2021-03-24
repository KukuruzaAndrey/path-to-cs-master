(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* 1. This problem involves using first-name substitutions to come up with alternate names. For example,
Fredrick William Smith could also be Fred William Smith or Freddie William Smith. Only part (d) is
specifically about this, but the other problems are helpful. *)

(* (a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. Sample solution is around 8 lines. *)
fun all_except_option (str, strings) =
    let
        fun iter (strings, acc) =
            case strings of
                []             => NONE
              | head::strings' => if same_string(head, str)
                                  then SOME (acc @ strings')
                                  else iter(strings', head::acc)
    in
        iter(strings, [])
    end

(* (b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result. Example:
get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred")
(* answer: ["Fredrick","Freddie","F"] *)
Assume each list in substitutions has no repeats. The result will have repeats if s and another string are
both in more than one list in substitutions. Example:
get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff")
(* answer: ["Jeffrey","Geoff","Jeffrey"] *)
Use part (a) and ML’s list-append (@) but no other helper functions. Sample solution is around 6 lines. *)
fun get_substitutions1(substitutions, s) =
     case substitutions of
         []          => []
       | head::subs' => case all_except_option (s, head) of
                            NONE      => get_substitutions1(subs', s)
                          | SOME strs => strs @ get_substitutions1(subs', s)

(* (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function. *)
fun get_substitutions2 (substitutions, s) =
    let
        fun aux(subs, acc) =
            case subs of
                []          => acc
              | head::subs' => case all_except_option (s, head) of
                                   NONE      => aux(subs', acc)
                                 | SOME strs => aux(subs', acc @ strs)
    in
        aux(substitutions, [])
    end

(* (d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names). Example:
similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],
{first="Fred", middle="W", last="Smith"})
(* answer: [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
{first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}] *)
Do not eliminate duplicates from the answer. Hint: Use a local helper function. Sample solution is
around 10 lines. *)
fun similar_names (substitutions, {first, middle, last}) =
    let
        val subs = get_substitutions2(substitutions, first)
        fun aux (substitutions) =
            case substitutions of
                []          => []
              | head::subs' => {first=head, middle=middle, last=last} :: aux(subs')
    in
        {first=first, middle=middle, last=last} :: aux(subs)
    end


(* 2. This problem involves a solitaire card game invented just for this question. You will write a program that
tracks the progress of a game; writing a game player is a challenge problem. You can do parts (a)–(e) before
understanding the game if you wish.
A game is played with a card-list and a goal. The player has a list of held-cards, initially empty. The player
makes a move by either drawing, which means removing the first card in the card-list from the card-list and
adding it to the held-cards, or discarding, which means choosing one of the held-cards to remove. The game
ends either when the player chooses to make no more moves or when the sum of the values of the held-cards
is greater than the goal.
The objective is to end the game with a low score (0 is best). Scoring works as follows: Let sum be the sum
of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum−goal),
else the preliminary score is (goal − sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual
with integer division; use ML’s div operator). *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* (a) Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red). Note: One case-expression is enough. *)
fun card_color (suit, _) =
    case suit of
        Clubs    => Black
      | Spades   => Black
      | Hearts   => Red
      | Diamonds => Red

(* (b) Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)
fun card_value (_, rank) =
    case rank of
        Jack  => 10
      | Queen => 10
      | King  => 10
      | Ace   => 11
      | Num n => n

(* (c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. You can compare cards with =. *)
fun remove_card (cs, c, e) =
    case cs of
        []        => raise e
      | head::cs' => if (head = c)
                     then cs'
                     else head :: remove_card(cs', c, e)

(* (d) Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color. Hint: An elegant solution is very similar to one of the functions using nested
pattern-matching in the lectures. *)
fun all_same_color cs =
    case cs of
        f::s::cs' => card_color f = card_color s andalso all_same_color(s::cs')
      | f::[]     => true
      | []        => true

(* (e) Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
defined helper function that is tail recursive. (Take “calls use a constant amount of stack space” as a
requirement for this problem.) *)
fun sum_cards cs =
    let
        fun aux (cs, acc) =
            case cs of
                [] => acc
              | head::cs' => aux(cs', acc + card_value head)
    in
        aux(cs, 0)
    end

(* (f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
the score as described above. *)
fun score (cs, goal) =
    let
        val sum = sum_cards cs
        val preliminary_score = if sum > goal
                                then 3 * (sum - goal)
                                else goal - sum
    in
        if all_same_color cs
        then preliminary_score div 2
        else preliminary_score
    end

(* (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game. As
described above:
• The game starts with the held-cards being the empty list.
• The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
• If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
• If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
with a larger held-cards and a smaller card-list.
Sample solution for (g) is under 20 lines. *)
fun officiate (cs, ms, goal) =
    let
        fun step (ms, cs, held_cards) =
            case (ms, cs) of
                ([], _)               => held_cards
              | ((Discard c)::ms', _) => step(ms', cs, remove_card(held_cards, c, IllegalMove))
              | (Draw::_, [])         => held_cards
              | (Draw::ms', c::cs')   => if sum_cards(c::held_cards) > goal
                                         then c::held_cards
                                         else step(ms', cs', c::held_cards)
    in
        score(step(ms, cs, []), goal)
    end

(* 3. Challenge Problems:
(a) Write score_challenge and officiate_challenge to be like their non-challenge counterparts except
each ace can have a value of 1 or 11 and score_challenge should always return the least (i.e., best)
possible score. (Note the game-ends-if-sum-exceeds-goal rule should apply only if there is no sum that
is less than or equal to the goal.) Hint: This is easier than you might think. *)
fun score_challenge (cs, goal) =
    let
        fun count (cs, acc) =
            case cs of
                []               => score(acc, goal)
              | (suit, Ace)::cs' => let
                                        val one_sum = count(cs', (suit, Num 1)::acc)
                                        val eleven_sum = count(cs', (suit, Ace)::acc)
                                    in
                                            Int.min(one_sum, eleven_sum)
                                    end
              | c::cs'           => count(cs', c::acc)
    in
        count (cs, [])
    end

fun officiate_challenge (cs, ms, goal) =
    let
        fun sum_with_ace_as_one cs =
            case cs of
                []            => 0
              | (_, Ace)::cs' => sum_with_ace_as_one cs' + 1
              | c::cs'        => sum_with_ace_as_one cs' + card_value c
        fun step (ms, cs, held_cards) =
            case (ms, cs) of
                ([], _)               => score_challenge(held_cards, goal)
              | ((Discard c)::ms', _) => step(ms', cs, remove_card(held_cards, c, IllegalMove))
              | (Draw::_, [])         => score_challenge(held_cards, goal)
              | (Draw::ms', c::cs')   => if sum_with_ace_as_one(c::held_cards) > goal
                                         then score_challenge(c::held_cards, goal)
                                         else step(ms', cs', c::held_cards)
    in
        step(ms, cs, [])
    end

(* (b) Write careful_player, which takes a card-list and a goal and returns a move-list such that calling
officiate with the card-list, the goal, and the move-list has this behavior:
• The value of the held cards never exceeds the goal.
• A card is drawn whenever the goal is more than 10 greater than the value of the held cards. As a
detail, you should (attempt to) draw, even if no cards remain in the card-list.
• If a score of 0 is reached, there must be no more moves.
• If it is possible to reach a score of 0 by discarding a card followed by drawing a card, then this
must be done. Note careful_player will have to look ahead to the next card, which in many card
games is considered “cheating.” Also note that the previous requirement takes precedence: There
must be no more moves after a score of 0 is reached even if there is another way to get back to 0.
Notes:
• There may be more than one result that meets the requirements above. The autograder should
work for any correct strategy — it checks that the result meets the requirements.
• This problem is not a continuation of problem 3(a). In this problem, all aces have a value of 11. *)   
fun careful_player(cs, goal) =
    let
        fun has_card_with_value (cs, value) =
            case cs of
                []      => false
              | c::rest => (card_value c = value) orelse has_card_with_value(rest, value)
        fun find_card_with_value (cs, value, e) =
            case cs of
                []      => raise e
              | c::rest => if card_value c = value
                           then c
                           else find_card_with_value(rest, value, e)
        fun step(card_list, held_cards) =
            case(card_list, sum_cards held_cards) of
               ([], _)                 => if goal - sum_cards held_cards > 10
                                          then [Draw]
                                          else []
              | (card::card_list', 0)  => if goal - sum_cards held_cards > 10
                                          then Draw::step(card_list', card::held_cards)
                                          else []   
              | (card::card_list', _)  => if goal - sum_cards held_cards > 10
                                          then Draw::step(card_list', card::held_cards)
                                          else
                                              let
                                                  val need_fold_value = sum_cards(card::held_cards) - goal
                                              in
                                                  if has_card_with_value(held_cards, need_fold_value)
                                                  then
                                                      let
                                                          val discard_card = find_card_with_value(held_cards, need_fold_value, IllegalMove)
                                                      in
                                                          [Discard discard_card, Draw]
                                                      end
                                                  else []
                                              end
    in
        step(cs, [])
    end
