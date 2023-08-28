:- include('./input.pl').

monthly_consumptions(Monthly_consumptions) :- findall((Month,Amount), monthly_consumption(Month,Amount), Monthly_consumptions).

% sum op monthly_consumptions
sum_of_monthly_consumptions([],0).
sum_of_monthly_consumptions([(Month,Amount)|T], Sum) :-
    sum_of_monthly_consumptions(T, Rest),
    Sum is Amount + Rest.

% only true if the monhtly consumptions are between 1 and 12, there are twelwe of them, and they are all different, where montly consumption is an array of (month, amount) tuples
valid_monthly_consumptions(Monthly_consumptions) :-
    length(Monthly_consumptions, 12),
    valid_monthly_consumptions(Monthly_consumptions, []).

valid_monthly_consumptions([], _).
valid_monthly_consumptions([(Month,_)|T], Months) :-
    \+ member(Month, Months),
    valid_monthly_consumptions(T, [Month|Months]).




rolling_consumption(Consumption) :-
    consumption(Consumption),
    Consumption > 20,
    Consumption < 30.

start:- 
    monthly_consumptions(Consumptions),
    write_ln(Consumptions),
    valid_monthly_consumptions(Consumptions),
    sum_of_monthly_consumptions(Consumptions,Sum),
    write(Sum),
    halt(0).

%write all elements of monthly_consumptions
%start:- monthly_consumptions, write(Consumptions), halt(0).


% https://stackoverflow.com/questions/9875760/sum-of-elements-in-list-in-prolog
sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.