fun is_older (date1: int*int*int, date2: int*int*int) =
    let
        val y = #1 date1 - #1 date2;
        val m = #2 date1 - #2 date2;
        val d = #3 date1 - #3 date2;
    in
        if y <> 0
        then y < 0
        else
            if m <> 0
            then m < 0
            else
                if d <> 0
                then d < 0
                else false
    end

fun number_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else if #2 (hd dates) = month
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
	
fun dates_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then []
    else if #2 (hd dates) = month
    then (hd dates)::dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null dates
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))
							   
fun get_nth (strings: (string) list, n: int) =
    if n = 1
    then
	hd strings
    else
	get_nth(tl strings, n - 1)
	       
fun date_to_string (date:int * int * int) =
    let
	val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
    in
	get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum (sum: int, inlist: int list) =
    let
	fun sums (n: int, count: int, inlist: int list) =
	    if (count + (hd inlist)) >= sum
	    then n
	    else
		sums(n+1, count + hd inlist, tl inlist)
    in
	sums(0, 0, inlist)
    end
	
fun what_month (day: int) =
    let
	val months = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	1 + number_before_reaching_sum(day, months)
    end
	
fun month_range (day1: int, day2: int) =
    if day2 < day1
    then []
    else
	what_month(day1)::month_range(day1 + 1, day2)
		      
fun oldest (dates: (int*int*int) list) = 
    if null dates
    then NONE
    else
        let
            fun older_than(dates: (int*int*int) list, oldest_date: int*int*int) =
                if null dates
                then oldest_date
                else
                    let
                        val current_date = hd dates
                    in
                        if is_older(current_date, oldest_date)
                        then older_than(tl dates, current_date)
                        else older_than(tl dates, oldest_date)
                    end
        in
            SOME (older_than(tl dates, hd dates))
        end


		    
				       
