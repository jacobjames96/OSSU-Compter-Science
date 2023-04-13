
fun is_older (dt1: int*int*int, dt2: int*int*int) =
    let
	val y1 = #1 dt1
	val m1 = #2 dt1
	val d1 = #3 dt1
	val y2 = #1 dt2
	val m2 = #2 dt2
	val d2 = #3 dt2
    in
	y1 < y2
	orelse (y1 = y2 andalso m1 < m2)
	orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    end;

fun number_in_month (dts: (int*int*int) list, month: int) =
    if null dts
    then 0
    else (if (#2 (hd dts)) = month then 1 else 0) + number_in_month((tl dts), month);

fun number_in_months (dts: (int*int*int) list, mths: int list) =
    if null mths
    then 0
    else number_in_month(dts, hd mths) + number_in_months(dts, tl mths);

fun dates_in_month (dts: (int*int*int) list, month: int) =
    if null dts
    then []
    else
	if (#2 (hd dts)) = month then (hd dts)::dates_in_month(tl dts, month)
	else dates_in_month(tl dts, month);

fun dates_in_months (dts: (int*int*int) list, mths: int list) =
    if null mths
    then []
    else dates_in_month(dts, hd mths) @ dates_in_months(dts, tl mths);

fun get_nth (sts: string list, n: int) =
    if n = 1
    then hd sts
    else get_nth(tl sts, n-1);

fun date_to_string (dt: int*int*int) =
    let val mths = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
    in
	get_nth(mths, (#2 dt)) ^ " " ^ Int.toString(#3 dt) ^ ", " ^ Int.toString(#1 dt)
    end;

fun number_before_reaching_sum (sum: int, xs: int list) =
    if (hd xs) >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - (hd xs), (tl xs));

fun what_month (d: int) =
    let val mths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(d, mths) + 1
    end;

fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1+1,day2);

fun oldest (dts: (int*int*int) list) =
    if null dts
    then NONE
    else
	let
	    fun oldest_nonempty (dts: (int*int*int) list) =
		if null (tl dts)
		then hd dts
		else let val tl_ans = oldest_nonempty(tl dts)
		     in
			 if is_older(hd dts, tl_ans)
			 then hd dts
			 else tl_ans
		     end
	in
	    SOME (oldest_nonempty dts)
	end;
