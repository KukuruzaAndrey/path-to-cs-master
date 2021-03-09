(* You will write 11 SML functions (and tests for them) related to calendar dates.  In all problems, a “date”
is an SML value of typeint*int*int, where the first part is the year, the second part is the month, and
the third part is the day.  A “reasonable” date has a positive year, a month between 1 and 12, and a day no
greater than 31 (or less depending on the month).  Your solutions need to work correctly only for reasonable
dates, but do not check for reasonable dates (that is a challenge problem) and many of your functions will
naturally  work  correctly  for  some/all  non-reasonable  dates.   A  “day  of  year”  is  a  number  from  1  to  365
where, for example, 33 represents February 2.  (We ignore leap years except in one challenge problem.) *)

(* 1.  Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument.  (If the two dates are the same, the result is false.) *)
fun is_older (date1:int*int*int, date2:int*int*int) =
    #1 date1 < #1 date2
    orelse (#1 date1 = #1 date2 andalso #2 date1 < #2 date2)
    orelse (#1 date1 = #1 date2 andalso #2 date1 = #2 date2 andalso #3 date1 < #3 date2)

(* 2.  Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns how many dates in the list are in the given month. *)
fun number_in_month (dates: (int*int*int) list, month: int) =
	if null dates
	then 0
	else
		(if #2 (hd dates) = month
		then 1
		else 0)
		+ number_in_month(tl dates, month)

(* 3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list) and returns the number of dates in the list of dates that are in any of the months in the list of months. Assume the list of months has no number repeated. Hint:  Use your answer to the previous problem. *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
	if null months
	then 0
	else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* 4.  Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a list holding the dates from the argument list of dates that are in the month.  The returned list should contain dates in the order they were originally given. *)
fun dates_in_month (dates: (int*int*int) list, month: int) =
	if null dates
	then []
	else if #2 (hd dates) = month then (hd dates)::dates_in_month(tl dates, month) else dates_in_month(tl dates, month)





