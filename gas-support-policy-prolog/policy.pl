:- include('./input.pl').
:- include('./matrix.pl').

% Flow: 
% 1. sum individual past consumptions
% 2. calculate rolling consumption
% 3. classify using rolling consumpiton
% 4. classify using currently saved amount compared to rollingconsumption
% 5. apply savings based support to base payement
% 6. apply social credential based support 

% === 1. Aggregate past consumptions ===
monthly_consumptions(Monthly_consumptions) :- findall((Month,Amount), monthly_consumption(Month,Amount), Monthly_consumptions).

% only true if the monhtly consumptions are between 1 and 12, there are twelwe of them, and they are all different, where montly consumption is an array of (month, amount) tuples
valid_monthly_consumptions(Monthly_consumptions) :-
    length(Monthly_consumptions, 12),
    valid_monthly_consumptions(Monthly_consumptions, []).

valid_monthly_consumptions([], _).
valid_monthly_consumptions([(Month,_)|T], Months) :-
    \+ member(Month, Months), % should we remove this negation?
    valid_monthly_consumptions(T, [Month|Months]).

% sum op monthly_consumptions
sum_of_monthly_consumptions([],0).
sum_of_monthly_consumptions([(_,Amount)|T], Sum) :-
    sum_of_monthly_consumptions(T, Rest),
    Sum is Amount + Rest.

% === 2. Calculate rolling consumption ===
rolling_consumption(Consumptions, Result):- 
    sum_of_monthly_consumptions(Consumptions,Sum),
    Result is Sum / 12.

% === 3. classify using rolling consumption ===
consumptionClass(Consumptions, Class):-
    rolling_consumption(Consumptions, Rolling_consumption),
    calcConsumptionClass(Rolling_consumption, Class).
    
calcConsumptionClass(Rolling_consumption,Class):-
    rolling_treshold('high',Treshold),
    Rolling_consumption >= Treshold,
    Class = 'high';
    rolling_treshold('mid',Treshold),
    Rolling_consumption >= Treshold,
    Class = 'mid';
    Class = 'low'.

% === 4. classify using savings ===
savingsClass(Class,Consumptions):-
    currentConsumption(Consumption),
    rolling_consumption(Consumptions, Rolling_consumption),
    calcSavingsClass(Rolling_consumption,Consumption,Class).

calcSavingsClass(Rolling_consumption,Consumption,Class):-
    savings_treshold('high',Treshold),
    CurrentSaving is Rolling_consumption - Consumption,
    CurrentSaving > Treshold,
    Class = 'high';
    savings_treshold('mid',Treshold),
    CurrentSaving is Rolling_consumption - Consumption,
    CurrentSaving > Treshold,
    Class = 'mid';
    savings_treshold('low',Treshold),
    CurrentSaving is Rolling_consumption - Consumption,
    CurrentSaving > Treshold,
    Class = 'low';
    Class = 'none'.

% === 5. Apply savings based support ===
paymentBase(Payment):-
    currentConsumption(Amount),
    currentPrice(Price,'HUF'),
    Payment is Price * Amount.

applySupport(Input, 'nominal', Value, Output):-
    Output is Input - Value.
applySupport(Input, 'percent', Value, Output):-
    AmountToBeSubtracted is Input * Value div 100,
    Output is Input - AmountToBeSubtracted.

applySavingsSupport(Input, RollingClass, SavingsClass, Output):-
    support_matrix(RollingClass, SavingsClass, Type, Value),
    applySupport(Input, Type, Value, Output).

% === 6. Apply social standing based support ===
socialCreds(Creds) :- findall((CredType,SupportType,SupportValue), social_suport(CredType,SupportType,SupportValue), Creds).

applySocialSupports(Input,[], Input).
applySocialSupports(Input,[(_,SupportType,SupportValue)|CredsTail],Result):-
    applySupport(Input, SupportType, SupportValue,Output),
    applySocialSupports(Output,CredsTail,Result).

calculatedPayment(PaymentAfterSocialSupports):-
    monthly_consumptions(Consumptions),
    valid_monthly_consumptions(Consumptions),
    consumptionClass(Consumptions,RollingClass),
    write('ConsumptionClass: '),
    writeln(RollingClass),
    savingsClass(SavingsClass,Consumptions),
    write('Savings class: '),
    writeln(SavingsClass),
    paymentBase(BasePayment),
    write('Base payment : '),
    writeln(BasePayment),
    applySavingsSupport(BasePayment,RollingClass,SavingsClass, PaymentAfterSavings),
    write('Payment after savings: '),
    writeln(PaymentAfterSavings),
    socialCreds(SocialCreds),
    write('SocialCreds:'),
    writeln(SocialCreds),
    applySocialSupports(PaymentAfterSavings,SocialCreds,PaymentAfterSocialSupports),
    write('Payment after Social supports: '),
    writeln(PaymentAfterSocialSupports).

inputPaymentOk:-
    calculatedPayment(Payment),
    inputPayment(Payment).

start:- 
    calculatedPayment(_),
    inputPaymentOk,
    writeln('Input payment and calcualted payment match!'),
    halt(0).



