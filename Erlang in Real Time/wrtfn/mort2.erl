-module(mort2).
-export([mort/4]).
-import(math, [log/1]).

% Calculate the number of repayments required to pay
% a mortgage of Amt repaid at NumRep payments of Repay per 
% year with interest taken NumRep times using an annual
% interest rate of Rate

mort(Amt, Rate, NumRep, Repay) ->
	AddRate = Rate / 100.0 / NumRep,
	repayments(Amt, AddRate, Repay).

repayments(Loan, Rate, Payment) 
	when Loan >= 0, Rate == 0, Payment > Loan*Rate ->
	ceiling(Loan/Payment);
repayments(Loan, Rate, Payment)
	when Loan >= 0, Rate > 0, Payment > Loan*Rate ->
	ceiling(-log(1.0 - Rate*Loan/Payment)/log(1.0 + Rate)).

ceiling(X) ->
	N = trunc(X),
	if 
		N < X -> N+1;
     		N >= X -> N
	end.

