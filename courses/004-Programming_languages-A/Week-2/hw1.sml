(* You will write 11 SML functions (and tests for them) related to calendar dates. In all problems, a “date”
is an SML value of type int*int*int, where the first part is the year, the second part is the month, and
the third part is the day. A “reasonable” date has a positive year, a month between 1 and 12, and a day no
greater than 31 (or less depending on the month). Your solutions need to work correctly only for reasonable
dates, but do not check for reasonable dates (that is a challenge problem) and many of your functions will
naturally work correctly for some/all non-reasonable dates. A “day of year” is a number from 1 to 365
where, for example, 33 represents February 2. (We ignore leap years except in one challenge problem.) *)

(* 1. Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.) *)
fun is_older (date1:int*int*int, date2:int*int*int) =
    #1 date1 < #1 date2
    orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2)
    orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)
fun number_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
        (if #2 (hd dates) = month
        then 1
        else 0)
        + number_in_month(tl dates, month)

(* 3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)
fun dates_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else
        if #2 (hd dates) = month
        then (hd dates) :: dates_in_month(tl dates, month)
        else dates_in_month(tl dates, month)

(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). *)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(* 6. Write a function get_nth that takes a list of strings and an int n and returns the n-th element of the
list where the head of the list is 1st. Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay. *)
fun get_nth (strings: string list, n: int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)

(* 7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December. *)
val months_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
fun date_to_string (date: int*int*int) =
    get_nth(months_names, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

(* 8. Write a function number_before_reaching_sum that takes an int called sum, which you can assume
is positive, and an int list, which you can assume contains all positive numbers, and returns an int.
You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)
fun number_before_reaching_sum (sum: int, numbers: int list) =
     



