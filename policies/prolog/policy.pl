% policy.pl 

% Flow: 
% 1. sum individual past consumptions
% 2. calculate rolling consumption
% 3. classify using rolling consumpiton
% 4. classify using currently saved amount compared to rollingconsumption
% 5. apply savings based support to base payement
% 6. apply social credential based support 

% === 1. Aggregate past consumptions ===

% monthlyConsumptions(MonthlyConsumptions) :- findall((Amount), monthly_consumption(_,Amount), MonthlyConsumptions).
% monthlyConsumptions(MonthlyConsumptions) :- MonthlyConsumptions = [(1,2001),(2,2001),(3,2001),(4,2001),(5,2001),(6,2001),(7,2000),(8,2000),(9,2000),(10,2000),(11,2000),(12,2000)].
monthlyConsumptions(MonthlyConsumptions) :- MonthlyConsumptions = [2001,2001,2001,2001,2001,2001,2000,2000,2000,2000,2000,2000].

sumOfMonthlyConsumptions([Amount|Tail],Sum) :- sumOfMonthlyConsumptions(Tail,SumOfTail), Sum is SumOfTail + Amount.
sumOfMonthlyConsumptions([],0).

% === 2. Calculate rolling consumption ===
rollingConsumption(Sum,Result):- 
    Result is Sum div 12.

% === 3. classify using rolling consumption ===   
consumptionClass(RollingConsumption,Class):-
    rolling_treshold('high',Treshold),
    RollingConsumption > Treshold,
    Class = 'high'.
consumptionClass(RollingConsumption,Class):-
    rolling_treshold('mid',Treshold),
    RollingConsumption > Treshold,
    Class = 'mid'.
consumptionClass(_RollingConsumption,Class):-
    Class = 'low'.

% === 4. classify using savings ===
savingsClass(RollingConsumption,Consumption,Class):-
    savings_treshold('high',Treshold),
    CurrentSaving is RollingConsumption - Consumption,
    CurrentSaving > Treshold,
    Class = 'high'.
savingsClass(RollingConsumption,Consumption,Class):-
    savings_treshold('mid',Treshold),
    CurrentSaving is RollingConsumption - Consumption,
    CurrentSaving > Treshold,
    Class = 'mid'.
savingsClass(RollingConsumption,Consumption,Class):-
    savings_treshold('low',Treshold),
    CurrentSaving is RollingConsumption - Consumption,
    CurrentSaving > Treshold,
    Class = 'low'.
savingsClass(_RollingConsumption,_Consumption,Class):-
    Class = 'none'.

% === 5. Apply savings based support ===
priceBase(PriceBase):-
    currentConsumption(Amount),
    currentPrice(Price,'HUF'),
    PriceBase is Price * Amount.

applySupport(ApplySupportInput, nominal, ApplySupportValue, ApplySupportOutput):-
    ApplySupportOutput is ApplySupportInput - ApplySupportValue.
applySupport(ApplySupportInput, percent, ApplySupportValue, ApplySupportOutput):-
    AmountToBeSubtracted is ApplySupportInput * ApplySupportValue div 100,
    ApplySupportOutput is ApplySupportInput - AmountToBeSubtracted.

applySavingsSupport(ApplySavingsSupportInput, ApplySavingsSupportSavingsClass, ApplySavingsSupportRollingClass, ApplySavingsSupportOutput):-
    support_matrix(ApplySavingsSupportRollingClass, ApplySavingsSupportSavingsClass, ApplySavingsSupportType, ApplySavingsSupportValue),
    applySupport(ApplySavingsSupportInput, ApplySavingsSupportType, ApplySavingsSupportValue, ApplySavingsSupportOutput).

% === 6. Apply social standing based support ===
socialCreds(Creds):- Creds = [('ChangedWorkcapacityCredential',nominal,10000)].
% socialCreds(Creds) :- findall((CredType,SupportType,SupportValue), social_suport(CredType,SupportType,SupportValue), Creds).

applySocialSupports(Input,[(_,SupportType,SupportValue)|CredsTail],Result):-
    applySupport(Input, SupportType, SupportValue,Output),
    applySocialSupports(Output,CredsTail,Result).
applySocialSupports(Input,[], Input).

endPrice(Price):-
    monthlyConsumptions(MonthlyConsumptions),
    sumOfMonthlyConsumptions(MonthlyConsumptions,Sum),
    rollingConsumption(Sum,RollingConsumption),
    currentConsumption(Consumption),
    consumptionClass(RollingConsumption,ConsumptionClass),
    savingsClass(RollingConsumption,Consumption,SavingsClass),
    priceBase(PriceBase),
    applySavingsSupport(PriceBase, SavingsClass, ConsumptionClass, PriceAfterSavings),
    socialCreds(Creds),
    applySocialSupports(PriceAfterSavings,Creds,Price).

inputPriceOk:-
    endPrice(Price),
    inputPayment(Price).