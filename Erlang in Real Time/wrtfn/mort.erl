-module(mort).
-export([mort/4]).

% Calculate the number of repayments required to pay
% a mortgage of Amt repaid at NumRep payments of Repay per 
% year with interest taken NumRep times using an annual
% interest rate of Rate

mort(Amt, Rate, NumRep, Repay) ->
	AddRate = Rate / 100.0 / NumRep,
	cnt(Amt, Repay, AddRate, 0).

cnt(Outs, Sub, AddRate, N) when Outs =< 0.0 ->
	N;
cnt(Outs, Sub, AddRate, N) ->
	Add = Outs * AddRate,
	cnt(Outs - Sub + Add, Sub, AddRate, N+1).
