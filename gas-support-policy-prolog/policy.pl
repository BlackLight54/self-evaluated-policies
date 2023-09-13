% policy.pl 
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
monthlyConsumptions(MonthlyConsumptions) :- findall((Month,Amount), monthly_consumption(Month,Amount), MonthlyConsumptions).

% sum op monthlyConsumptions
sumOfMonthlyConsumptions([],0).
sumOfMonthlyConsumptions([(_,Amount)|T], Sum) :-
    sumOfMonthlyConsumptions(T, Rest),
    Sum is Amount + Rest.

% === 2. Calculate rolling consumption ===
rollingConsumption(Result):- 
    monthlyConsumptions(Consumptions),
    sumOfMonthlyConsumptions(Consumptions,Sum),
    Result is Sum / 12.

% === 3. classify using rolling consumption ===
consumptionClass(Class):-
    rollingConsumption(RollingConsumption),
    calcConsumptionClass(RollingConsumption, Class).
    
calcConsumptionClass(RollingConsumption,Class):-
    rolling_treshold('high',Treshold),
    RollingConsumption >= Treshold,
    Class = 'high';
    rolling_treshold('mid',Treshold),
    RollingConsumption >= Treshold,
    Class = 'mid';
    Class = 'low'.

% === 4. classify using savings ===
savingsClass(Class):-
    currentConsumption(Consumption),
    rollingConsumption(RollingConsumption),
    calcSavingsClass(RollingConsumption,Consumption,Class).

calcSavingsClass(RollingConsumption,Consumption,Class):-
    savings_treshold('high',Treshold),
    CurrentSaving is RollingConsumption - Consumption,
    CurrentSaving > Treshold,
    Class = 'high';
    savings_treshold('mid',Treshold),
    CurrentSaving is RollingConsumption - Consumption,
    CurrentSaving > Treshold,
    Class = 'mid';
    savings_treshold('low',Treshold),
    CurrentSaving is RollingConsumption - Consumption,
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

applySavingsSupport(Output):-
    paymentBase(Input), 
    savingsClass(SavingsClass),
    consumptionClass(RollingClass),
    support_matrix(RollingClass, SavingsClass, Type, Value),
    applySupport(Input, Type, Value, Output).

% === 6. Apply social standing based support ===
socialCreds(Creds) :- findall((CredType,SupportType,SupportValue), social_suport(CredType,SupportType,SupportValue), Creds).

applySocialSupports(Result):-
    applySavingsSupport(Input),
    socialCreds(Creds),
    applySocialSupports(Input,Creds,Result).
applySocialSupports(Input,[], Input).
applySocialSupports(Input,[(_,SupportType,SupportValue)|CredsTail],Result):-
    applySupport(Input, SupportType, SupportValue,Output),
    applySocialSupports(Output,CredsTail,Result).

calculatedPayment(Payment):-
    applySocialSupports(Payment).

inputPaymentOk:-
    calculatedPayment(Payment),
    inputPayment(Payment).

writeSteps:-
    monthlyConsumptions(MonthlyConsumptions),
    write('Monthly consumptions:'),nl,
    write(MonthlyConsumptions),nl,
    rollingConsumption(RollingConsumption),
    write('Rolling consumption:'),nl,
    write(RollingConsumption),nl,
    consumptionClass(ConsumptionClass),
    write('Consumption class:'),nl,
    write(ConsumptionClass),nl,
    savingsClass(SavingsClass),
    write('Savings class:'),nl,
    write(SavingsClass),nl,
    paymentBase(Payment),
    write('Payment base:'),nl,
    write(Payment),nl,
    support_matrix(ConsumptionClass, SavingsClass, Type, Value),
    write('Value in savings support matrix:'),nl,
    write((ConsumptionClass, SavingsClass, Type, Value)),nl,
    applySavingsSupport(PaymentAfterSavings),
    write('Payment after savings support:'),nl,
    write(PaymentAfterSavings),nl,
    socialCreds(Creds),
    write('Social creds:'),nl,
    write(Creds),nl,
    applySocialSupports(PaymentAfterSocialSupports),
    write('Payment after social supports:'),nl,
    write(PaymentAfterSocialSupports).

start:- 
    calculatedPayment(_),
    inputPaymentOk,
    write('Input payment and calcualted payment match!'),nl,
    halt(0).



