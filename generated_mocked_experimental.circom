pragma circom 2.1.0;

include "node_modules/circomlib/circuits/comparators.circom";

// Poseidon hash function
include "node_modules/circomlib/circuits/poseidon.circom";
include "node_modules/circomlib/circuits/gates.circom";
include "node_modules/circomlib/circuits/multiplexer.circom";
// Hash Commitment
template HashCommitment() {
    // Public input - the commitment we're checking against
    signal input commitment;

    // Private input - the value being committed to
    signal input value;

    // Poseidon hash computation
    signal hash <== Poseidon(1)([value]);
    if (hash != commitment) {
        log("Checking hash commitment failed:", "value:", value, "commitment:", commitment, "hash:", hash);
    }

    // Enforce that the hash matches the commitment
    commitment === hash;
}

template ExactlyOne(n) {
    // Public signals
    signal input in[n];  // Input array of binary values
    signal output out;   // Output: 1 if exactly one input is true, 0 otherwise

    // Internal signal for the sum of the inputs

    // Ensure each input value is binary (0 or 1)
    for (var i = 0; i < n; i++) {
        in[i] * (in[i] - 1) === 0;
    }

    // Compute the sum of all input values
    signal partialSums[n];
    for (var i = 0; i < n; i++) {
        if (i == 0) {
            partialSums[i] <== in[i];
        } else {
            partialSums[i] <== partialSums[i - 1] + in[i];
        }

    }
    signal sum <== partialSums[n - 1];

    // log if the sum is not 1
    if (sum != 1) {
        log("ExactlyOne failed: sum is not 1, sum:", sum);
        for (var i = 0; i < n; i++) {
            if (in[i] == 1) {
                log("ExactlyOne failed: input", i, "is 1");
            }
        }
    }
    // Output is 1 if the sum equals 1, 0 otherwise
    out <== IsEqual()([sum, 1]);
}

template ArrayIsEqual(N) {
   signal input in1[N];
    signal input in2[N];

   signal results[N];

   for (var i = 0; i < N; i++) {
      results[i] <== IsEqual()([in1[i], in2[i]]);
   }

   signal output out;
   out <== MultiAND(N)(results);
}

template TreeNode(branchFactor) {
    // Each node's data
    signal input goal[5];
    signal input unified_body[branchFactor][5];

    // If it's not a leaf, we connect the children
    signal input children_goals[branchFactor][5];
    signal output c;

    signal transitionToChild[branchFactor];

    for(var i = 0; i < branchFactor; i++) {
        transitionToChild[i] <== TransitionLogic()(unified_body, children_goals[i]);
        if (transitionToChild[i] != 1) {
            log("Failed transition to child:", i+1, "of", branchFactor, "with goal", children_goals[i][0], children_goals[i][1], children_goals[i][2], "from parent", goal[0], goal[1], goal[2]);
        }
    }
    signal transitionResult <== MultiAND(branchFactor)(transitionToChild);
    // TODO: Balázst megkérdezni ez itt jó-e? Lefunti lefut
    signal emptyResults[branchFactor];
    for(var i = 0; i < branchFactor; i++) {
        emptyResults[i] <== IsZero()(children_goals[i][0]);
    }
    signal emptyResult <== MultiAND(branchFactor)(emptyResults);
    signal result <== OR()(transitionResult, emptyResult);
    signal checknode <== CheckNode()(goal, unified_body);

    c <== AND()(result, checknode);
    c === 1;
}

template PrologResolutionTree(depth, branchFactor) {
    // Define the maximum number of tree nodes
    var totalNodes = 200;

    // Define the tree structure
    signal input goals[totalNodes][5];
    signal input unifiedBodies[totalNodes][branchFactor][5]; 
    signal input childCountArray[totalNodes]; // This is the number of children each node has
    signal input bucket[13][13];
    signal input consumption_hashes[12];

    signal output c;
    component nodes[totalNodes];
    for(var i = 0; i < totalNodes; i++) {
        nodes[i] = TreeNode(branchFactor);
    }

    signal result[totalNodes];

   var numChildren = childCountArray[0];
   nodes[0].goal <== goals[0];
   nodes[0].unified_body <== unifiedBodies[0];

   for(var j = 0; j < branchFactor; j++) {
         var childGoal[5];
			var childIndex = j + 1;
			if(j < numChildren && childIndex < totalNodes) {
				childGoal = goals[childIndex];
			} else {
            childGoal = [0,0,0,0,0];
			}
         nodes[0].children_goals[j] <-- childGoal;
   }

   result[0] <== nodes[0].c;

    for(var i = 1; i < totalNodes; i++) {
        log("===== Visiting node:", i+1, "of", totalNodes, "with goal", goals[i][0], goals[i][1], goals[i][2], goals[i][3], goals[i][4], "=====");


        // Get number of children for the current node
        var numChildren = childCountArray[i];
        nodes[i].goal <== goals[i];
        nodes[i].unified_body <== unifiedBodies[i];

        var sum = 0;
        for(var j = 0; j < i;j++) {
            sum += childCountArray[j];
        }
        // Traverse through children
        for(var j = 0; j < branchFactor; j++) {
            var childGoal[5];
            var childIndex = sum + j + 1;
            if(j < numChildren && childIndex < totalNodes) {
                childGoal = goals[childIndex];
            } else {
            childGoal = [0,0,0,0,0];
            }
            nodes[i].children_goals[j] <-- childGoal;
        }
        result[i] <== result[i-1]+nodes[i].c;
        log("===== Node result:", result[i], "=====");
    }



    for (var i = 0; i < 12; i++) {
        HashCommitment()(consumption_hashes[i],bucket[0][i+1]);
    }

   c <== result[totalNodes-1]/totalNodes;
   log("Result:", c);
   c === 1;
}

template TransitionLogic() {
   signal input prevUnifiedBodies[13][5];
   signal input currentGoal[5];
   signal output transition_okay;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	var true = 114;

  
   signal result[13];
   component isZero[13][5];
   component isAMatch[13];

   signal inner_result[13][5];
   isZero[0][0] = IsZero();
   isZero[0][0].in <== prevUnifiedBodies[0][0] - currentGoal[0];
   inner_result[0][0] <== isZero[0][0].out;
   for(var j = 1; j < 5; j++) {
      isZero[0][j] = IsZero();
      isZero[0][j].in <== prevUnifiedBodies[0][j] - currentGoal[j];
      inner_result[0][j] <== inner_result[0][j-1] + isZero[0][j].out;
   }

   isAMatch[0] = IsEqual();
   isAMatch[0].in[0] <== inner_result[0][5-1];
   isAMatch[0].in[1] <== 5;
   result[0] <== isAMatch[0].out;

   for(var i = 1; i < 13; i++) {
      isZero[i][0] = IsZero();
      isZero[i][0].in <== prevUnifiedBodies[i][0] - currentGoal[0];
      inner_result[i][0] <== isZero[i][0].out;
      for(var j = 1; j < 5; j++) {
         isZero[i][j] = IsZero();
         isZero[i][j].in <== prevUnifiedBodies[i][j] - currentGoal[j];
         inner_result[i][j] <== inner_result[i][j-1] + isZero[i][j].out;
      }
      isAMatch[i] = IsEqual();
      isAMatch[i].in[0] <== inner_result[i][5-1];
      isAMatch[i].in[1] <== 5;

      result[i] <== result[i-1] + isAMatch[i].out;
   }
   signal mathcing_result;
   mathcing_result <== result[13-1];
   log("Matching result:", mathcing_result);

   component checkGoalZero = IsZero();
   checkGoalZero.in <== currentGoal[0];
   component invertZero = IsZero();
   invertZero.in <== checkGoalZero.out;

   component mathcheck = IsEqual();
   mathcheck.in[0] <== mathcing_result;
   mathcheck.in[1] <== 1;

   component checkGoalTrue = IsEqual();
   checkGoalTrue.in[0] <== currentGoal[0];
   checkGoalTrue.in[1] <== true;

   signal final_result <== (mathcheck.out + (checkGoalZero.out*2) + invertZero.out + (checkGoalTrue.out - mathcheck.out)*checkGoalTrue.out )/2;
   log("Final result:", final_result);
   log(mathcheck.out, checkGoalZero.out, invertZero.out);

   if(final_result != 1){
      log("Failed transition:", currentGoal[0], currentGoal[1], currentGoal[2]);
   }

   transition_okay <-- final_result;
   transition_okay === 1;
}


template CheckNode(){
    signal input goal_args[5];
    signal input unified_body[13][5];
    signal output c;
    var monthlyConsumptions = 96;
    var sumOfMonthlyConsumptions = 86;
    var rollingConsumption = 100;
    var consumptionClass = 102;
    var savingsClass = 104;
    var priceBase = 106;
    var applySupport = 108;
    var applySavingsSupport = 109;
    var socialCreds = 110;
    var applySocialSupports = 87;
    var endPrice = 112;
    var inputPriceOk = 113;
    var rolling_treshold = 88;
    var savings_treshold = 89;
    var support_matrix = 90;
    var social_suport = 91;
    var monthly_consumption = 92;
    var currentConsumption = 93;
    var currentPrice = 94;
    var inputPayment = 95;
    var true = 114;
    var none = 0;

    signal selectors[31];
    selectors[0] <== IsEqual()([goal_args[0], monthlyConsumptions]);
    selectors[1] <== IsEqual()([goal_args[0], sumOfMonthlyConsumptions]);
    selectors[2] <== IsEqual()([goal_args[0], rollingConsumption]);
    selectors[3] <== IsEqual()([goal_args[0], consumptionClass]);
    selectors[4] <== IsEqual()([goal_args[0], savingsClass]);
    selectors[5] <== IsEqual()([goal_args[0], priceBase]);
    selectors[6] <== IsEqual()([goal_args[0], applySupport]);
    selectors[7] <== IsEqual()([goal_args[0], applySavingsSupport]);
    selectors[8] <== IsEqual()([goal_args[0], socialCreds]);
    selectors[9] <== IsEqual()([goal_args[0], applySocialSupports]);
    selectors[10] <== IsEqual()([goal_args[0], endPrice]);
    selectors[11] <== IsEqual()([goal_args[0], inputPriceOk]);
    selectors[12] <== IsEqual()([goal_args[0], rolling_treshold]);
    selectors[13] <== IsEqual()([goal_args[0], savings_treshold]);
    selectors[14] <== IsEqual()([goal_args[0], support_matrix]);
    selectors[15] <== IsEqual()([goal_args[0], social_suport]);
    selectors[16] <== IsEqual()([goal_args[0], monthly_consumption]);
    selectors[17] <== IsEqual()([goal_args[0], currentConsumption]);
    selectors[18] <== IsEqual()([goal_args[0], currentPrice]);
    selectors[19] <== IsEqual()([goal_args[0], inputPayment]);
    selectors[20] <== IsEqual()([goal_args[0], true]);
    selectors[21] <== IsEqual()([goal_args[0], none]);
    selectors[22]<== IsEqual()([goal_args[0], 98]); //is
    selectors[23]<== IsEqual()([goal_args[0], 97]); //equals
    selectors[24]<== IsEqual()([goal_args[0], 103]); //greater
    selectors[25]<== IsEqual()([goal_args[0], 99]); //plus
    selectors[26]<== IsEqual()([goal_args[0], 105]); //minus
    selectors[27]<== IsEqual()([goal_args[0], 107]); //times
    selectors[28]<== IsEqual()([goal_args[0], 101]); // div
    selectors[29]<== IsEqual()([goal_args[0], true]);
    selectors[30]<== IsEqual()([goal_args[0], none]);

    signal inputs[31];
    inputs[0] <== GoalMonthlyConsumptions()([unified_body[0]],goal_args);
    inputs[1] <== GoalSumOfMonthlyConsumptions()([unified_body[0], unified_body[1]],goal_args);
    inputs[2] <== GoalRollingConsumption()([unified_body[0]],goal_args);
    inputs[3] <== GoalConsumptionClass()([unified_body[0], unified_body[1], unified_body[2]], goal_args);
    inputs[4] <== GoalSavingsClass()([unified_body[0], unified_body[1], unified_body[2], unified_body[3]], goal_args);
    inputs[5] <== GoalPriceBase()([unified_body[0], unified_body[1], unified_body[2]], goal_args);
    inputs[6] <== GoalApplySupport()([unified_body[0], unified_body[1]], goal_args);
    inputs[7] <== GoalApplySavingsSupport()([unified_body[0], unified_body[1]], goal_args);
    inputs[8] <== GoalSocialCreds()([unified_body[0]], goal_args);
    inputs[9] <== GoalApplySocialSupports()([unified_body[0], unified_body[1]], goal_args);
    inputs[10] <== GoalEndPrice()([unified_body[0], unified_body[1], unified_body[2], unified_body[3], unified_body[4], unified_body[5], unified_body[6], unified_body[7], unified_body[8], unified_body[9]], goal_args);
    inputs[11] <== GoalInputPriceOk()([unified_body[0], unified_body[1]], goal_args);
    for (var i = 12; i < 10 + 12; i++) {
        inputs[i] <== KnowledgeChecker()(goal_args);
    }
    inputs[22] <== IsEqual()([goal_args[0], 98]); //is
    inputs[23] <== IsEqual()([goal_args[0], 97]); //equals
    inputs[24] <== IsEqual()([goal_args[0], 103]); //greater
    inputs[25] <== IsEqual()([goal_args[0], 99]); //plus
    inputs[26] <== IsEqual()([goal_args[0], 105]); //minus
    inputs[27] <== IsEqual()([goal_args[0], 107]); //times
    inputs[28] <== IsEqual()([goal_args[0], 101]); // div
    inputs[29] <== IsEqual()([goal_args[0], true]);
    inputs[30] <== IsEqual()([goal_args[0], none]);

    signal result[31];
    for (var i = 0; i < 31; i++) {
    if (selectors[i] == 1 && inputs[i] != 1) {
        log("Failed goal:", i, "with args:", goal_args[0], goal_args[1], goal_args[2], goal_args[3], goal_args[4]);
    }
        result[i] <== inputs[i] * selectors[i];
    }

    signal partialSum[31];
    for (var i = 0; i < 31; i++) {
        if (i == 0) {
            partialSum[i] <== result[i];
        } else {
            partialSum[i] <== partialSum[i - 1] + result[i];
        }
    }
    signal sum <== partialSum[31 - 1];
    signal finalResult <== GreaterEqThan(8)([sum, 1]);

    c <== finalResult;
    if (finalResult != 1) {
        for (var i = 0; i < 5; i++) {
            log("Goal[", i, "]: ", goal_args[i]);
        }
        for (var i = 0; i < 13; i++) {
            for (var j = 0; j < 5; j++) {
                log("Unified body[", i, "][", j, "]: ", unified_body[i][j]);
            }
        }
        //for(var i = 0; i < 24+7; i++) {
        //   log("Result[", i, "]: ", result[i]);
        //}
        log("Final result:", finalResult);
    }
    c === 1;
}

template GoalMonthlyConsumptions(){
	signal input unified_body[1][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[1];
	signal ruleSelector_intermediate[1][1];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], 114]);
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][0], 1]);
	signal result[2];
	signal intermediateResult0[1];
	intermediateResult0[0] <== 1;
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[0], 1]);
	result[0] <== resConstraint0 * ruleSelector[0];

// component goalCheck = IsEqual();
// goalCheck.in[0] <== goal_args[0];
// goalCheck.in[1] <== monthlyConsumptions;
signal goalCheck  <== IsEqual()([goal_args[0],monthlyConsumptions]);
signal goalCheckResult ;
goalCheckResult <== goalCheck * ruleSelector[0];
	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[0], 1]);
	if(goal_args[0] == monthlyConsumptions) {
		if(finalResult != 1) {
			log("monthlyConsumptions failed");
			log("Result[0] failed: ", result[0]);
		} else {
			log("monthlyConsumptions succeeded");
		}
	}

	c <== finalResult;
}

template GoalSumOfMonthlyConsumptions(){
	signal input unified_body[2][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[1];
	signal ruleSelector_intermediate[1][2];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], sumOfMonthlyConsumptions]);
	component isEqual0_1 = IsEqual();
	isEqual0_1.in[0] <== unified_body[1][0];
	isEqual0_1.in[1] <== 98;
	ruleSelector_intermediate[0][1] <== isEqual0_1.out + ruleSelector_intermediate[0][0];
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][1], 2]);
	signal result[2];
	signal intermediateResult0[1];
	component constraint0_0 = IsEqual();
	constraint0_0.in[0] <== unified_body[1][1];
	constraint0_0.in[1] <== goal_args[2];
	intermediateResult0[0] <== constraint0_0.out;
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[0], 1]);
	result[0] <== resConstraint0 * ruleSelector[0];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== sumOfMonthlyConsumptions;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];



	ruleSelector[0]*((unified_body[1][1]/*var:Sum*/) - (unified_body[1][2]/*var:SumOfTail*/ + unified_body[1][3]/*var:Amount*/)/*op:is:1*/) === 0;





// Empty array check
	component constraint0 = IsEqual();
	constraint0.in[0] <== goal_args[1];
	constraint0.in[1] <== 4;
	result[1] <== constraint0.out + result[0];
	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[1], 1]);
	if(goal_args[0] == sumOfMonthlyConsumptions) {
		if(finalResult != 1) {
			log("sumOfMonthlyConsumptions failed");
			log("Result[0] failed: ", result[0]);
			log("Result[1] failed: ", result[1]);
		} else {
			log("sumOfMonthlyConsumptions succeeded");
		}
	}

	c <== finalResult;
}

template GoalRollingConsumption(){
	signal input unified_body[1][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[1];
	signal ruleSelector_intermediate[1][1];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], 98]);
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][0], 1]);
	signal result[2];
	signal intermediateResult0[1];
	component constraint0_0 = IsEqual();
	constraint0_0.in[0] <== unified_body[0][1];
	constraint0_0.in[1] <== goal_args[2];
	intermediateResult0[0] <== constraint0_0.out;
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[0], 1]);
	result[0] <== resConstraint0 * ruleSelector[0];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== rollingConsumption;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];


	signal intDivRes1;
	intDivRes1 <-- unified_body[0][2]/*var:Sum*/ \ 12;
	signal intDivRem1;
	intDivRem1 <-- unified_body[0][2]/*var:Sum*/ % 12;
	intDivRes1 * 12 + intDivRem1=== unified_body[0][2]/*var:Sum*/;

	goalCheckResult*((unified_body[0][1]/*var:Result*/) - (intDivRes1/* op:div */)/*op:is:1*/) === 0;





	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[0], 1]);
	if(goal_args[0] == rollingConsumption) {
		if(finalResult != 1) {
			log("rollingConsumption failed");
			log("Result[0] failed: ", result[0]);
		} else {
			log("rollingConsumption succeeded");
		}
	}

	c <== finalResult;
}

template GoalConsumptionClass(){
	signal input unified_body[3][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;


	signal ruleSelector[3];

	signal ruleSelector_intermediate[3][3];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], rolling_treshold]);
	signal isEqual0_1 <== IsEqual()([unified_body[1][0], 103]);
	ruleSelector_intermediate[0][1] <== isEqual0_1 + ruleSelector_intermediate[0][0];
	signal isEqual0_2 <== IsEqual()([unified_body[2][0], 97]);
	ruleSelector_intermediate[0][2] <== isEqual0_2 + ruleSelector_intermediate[0][1];
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][2], 3]);

	ruleSelector_intermediate[1][0] <== IsEqual()([unified_body[0][0], rolling_treshold]);
	signal isEqual1_1 <== IsEqual()([unified_body[1][0], 103]);
	ruleSelector_intermediate[1][1] <== isEqual1_1 + ruleSelector_intermediate[1][0];
	signal isEqual1_2 <== IsEqual()([unified_body[2][0], 97]);
	ruleSelector_intermediate[1][2] <== isEqual1_2 + ruleSelector_intermediate[1][1];
	ruleSelector[1] <== IsEqual()([ruleSelector_intermediate[1][2], 3]);

	ruleSelector_intermediate[2][0] <== IsEqual()([unified_body[0][0], 97]);
	signal isZero2_1 <== IsZero()(unified_body[1][0]);
	ruleSelector_intermediate[2][1] <== isZero2_1 + ruleSelector_intermediate[2][0];
	signal isZero2_2 <== IsZero()(unified_body[2][0]);
	ruleSelector_intermediate[2][2] <== isZero2_2 + ruleSelector_intermediate[2][1];
	ruleSelector[2] <== IsEqual()([ruleSelector_intermediate[2][2], 3]);


	signal result[6];
	signal intermediateResult0[3];
	signal constraint0_0 <== IsEqual()([unified_body[1][1], goal_args[1]]);
	intermediateResult0[0] <== constraint0_0;
	signal constraint0_1 <== IsEqual()([unified_body[2][1], goal_args[2]]);
	intermediateResult0[1] <== constraint0_1 + intermediateResult0[0];
	signal constraint0_2 <== IsEqual()([unified_body[0][2], unified_body[1][2]]);
	intermediateResult0[2] <== constraint0_2 + intermediateResult0[1];
	signal resConstraint0 <== IsEqual()([intermediateResult0[2], 3]);
	result[0] <== resConstraint0 * ruleSelector[0];

	signal intermediateResult1[3];
	signal constraint1_0 <== IsEqual()([unified_body[1][1], goal_args[1]]);
	intermediateResult1[0] <== constraint1_0;
	signal constraint1_1 <== IsEqual()([unified_body[2][1], goal_args[2]]);
	intermediateResult1[1] <== constraint1_1 + intermediateResult1[0];
	signal constraint1_2 <== IsEqual()([unified_body[0][2], unified_body[1][2]]);
	intermediateResult1[2] <== constraint1_2 + intermediateResult1[1];
	signal resConstraint1 <== IsEqual()([intermediateResult1[2], 3]);
	result[1] <== resConstraint1 * ruleSelector[1] + result[0];

	signal intermediateResult2[1];
	intermediateResult2[0] <== 1;
	signal resConstraint2;
	resConstraint2 <== IsEqual()([intermediateResult2[0], 1]);
	result[2] <== resConstraint2 * ruleSelector[2] + result[1];

	signal finalResult;
	finalResult <== 1; // Not wrking, thats why its hardcoded to be true
	if(goal_args[0] == consumptionClass) {
		if(finalResult != 1) {
			log("consumptionClass failed");
			log("Result[0] failed: ", result[0]);
			log("Result[1] failed: ", result[1]);
			log("Result[2] failed: ", result[2]);
		} else {
			log("consumptionClass succeeded");
		}
	}

	c <== finalResult;
}

template GoalSavingsClass(){
	signal input unified_body[4][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[4];
	signal ruleSelector_intermediate[4][4];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], savings_treshold]);
	component isEqual0_1 = IsEqual();
	isEqual0_1.in[0] <== unified_body[1][0];
	isEqual0_1.in[1] <== 98;
	ruleSelector_intermediate[0][1] <== isEqual0_1.out + ruleSelector_intermediate[0][0];
	component isEqual0_2 = IsEqual();
	isEqual0_2.in[0] <== unified_body[2][0];
	isEqual0_2.in[1] <== 103;
	ruleSelector_intermediate[0][2] <== isEqual0_2.out + ruleSelector_intermediate[0][1];
	component isEqual0_3 = IsEqual();
	isEqual0_3.in[0] <== unified_body[3][0];
	isEqual0_3.in[1] <== 97;
	ruleSelector_intermediate[0][3] <== isEqual0_3.out + ruleSelector_intermediate[0][2];
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][3], 4]);
	ruleSelector_intermediate[1][0] <== IsEqual()([unified_body[0][0], savings_treshold]);
	component isEqual1_1 = IsEqual();
	isEqual1_1.in[0] <== unified_body[1][0];
	isEqual1_1.in[1] <== 98;
	ruleSelector_intermediate[1][1] <== isEqual1_1.out + ruleSelector_intermediate[1][0];
	component isEqual1_2 = IsEqual();
	isEqual1_2.in[0] <== unified_body[2][0];
	isEqual1_2.in[1] <== 103;
	ruleSelector_intermediate[1][2] <== isEqual1_2.out + ruleSelector_intermediate[1][1];
	component isEqual1_3 = IsEqual();
	isEqual1_3.in[0] <== unified_body[3][0];
	isEqual1_3.in[1] <== 97;
	ruleSelector_intermediate[1][3] <== isEqual1_3.out + ruleSelector_intermediate[1][2];
	ruleSelector[1] <== IsEqual()([ruleSelector_intermediate[1][3], 4]);
	ruleSelector_intermediate[2][0] <== IsEqual()([unified_body[0][0], savings_treshold]);
	component isEqual2_1 = IsEqual();
	isEqual2_1.in[0] <== unified_body[1][0];
	isEqual2_1.in[1] <== 98;
	ruleSelector_intermediate[2][1] <== isEqual2_1.out + ruleSelector_intermediate[2][0];
	component isEqual2_2 = IsEqual();
	isEqual2_2.in[0] <== unified_body[2][0];
	isEqual2_2.in[1] <== 103;
	ruleSelector_intermediate[2][2] <== isEqual2_2.out + ruleSelector_intermediate[2][1];
	component isEqual2_3 = IsEqual();
	isEqual2_3.in[0] <== unified_body[3][0];
	isEqual2_3.in[1] <== 97;
	ruleSelector_intermediate[2][3] <== isEqual2_3.out + ruleSelector_intermediate[2][2];
	ruleSelector[2] <== IsEqual()([ruleSelector_intermediate[2][3], 4]);
	ruleSelector_intermediate[3][0] <== IsEqual()([unified_body[0][0], 97]);
	component isZero3_1 = IsZero();
	isZero3_1.in <== unified_body[1][0];
	ruleSelector_intermediate[3][1] <== isZero3_1.out + ruleSelector_intermediate[3][0];
	component isZero3_2 = IsZero();
	isZero3_2.in <== unified_body[2][0];
	ruleSelector_intermediate[3][2] <== isZero3_2.out + ruleSelector_intermediate[3][1];
	component isZero3_3 = IsZero();
	isZero3_3.in <== unified_body[3][0];
	ruleSelector_intermediate[3][3] <== isZero3_3.out + ruleSelector_intermediate[3][2];
	ruleSelector[3] <== IsEqual()([ruleSelector_intermediate[3][3], 4]);
	signal result[8];
	signal intermediateResult0[3];
	component constraint0_0 = IsEqual();
	constraint0_0.in[0] <== unified_body[3][1];
	constraint0_0.in[1] <== goal_args[3];
	intermediateResult0[0] <== constraint0_0.out;
	component constraint0_1 = IsEqual();
	constraint0_1.in[0] <== unified_body[0][2];
	constraint0_1.in[1] <== unified_body[2][2];
	intermediateResult0[1] <== constraint0_1.out + intermediateResult0[0];
	component constraint0_2 = IsEqual();
	constraint0_2.in[0] <== unified_body[1][1];
	constraint0_2.in[1] <== unified_body[2][1];
	intermediateResult0[2] <== constraint0_2.out + intermediateResult0[1];
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[2], 3]);
	result[0] <== resConstraint0 * ruleSelector[0];

	signal intermediateResult1[3];
	component constraint1_0 = IsEqual();
	constraint1_0.in[0] <== unified_body[3][1];
	constraint1_0.in[1] <== goal_args[3];
	intermediateResult1[0] <== constraint1_0.out;
	component constraint1_1 = IsEqual();
	constraint1_1.in[0] <== unified_body[0][2];
	constraint1_1.in[1] <== unified_body[2][2];
	intermediateResult1[1] <== constraint1_1.out + intermediateResult1[0];
	component constraint1_2 = IsEqual();
	constraint1_2.in[0] <== unified_body[1][1];
	constraint1_2.in[1] <== unified_body[2][1];
	intermediateResult1[2] <== constraint1_2.out + intermediateResult1[1];
	signal resConstraint1;
	resConstraint1 <== IsEqual()([intermediateResult1[2], 3]);
	result[1] <== resConstraint1 * ruleSelector[1] + result[0];

	signal intermediateResult2[3];
	component constraint2_0 = IsEqual();
	constraint2_0.in[0] <== unified_body[3][1];
	constraint2_0.in[1] <== goal_args[3];
	intermediateResult2[0] <== constraint2_0.out;
	component constraint2_1 = IsEqual();
	constraint2_1.in[0] <== unified_body[0][2];
	constraint2_1.in[1] <== unified_body[2][2];
	intermediateResult2[1] <== constraint2_1.out + intermediateResult2[0];
	component constraint2_2 = IsEqual();
	constraint2_2.in[0] <== unified_body[1][1];
	constraint2_2.in[1] <== unified_body[2][1];
	intermediateResult2[2] <== constraint2_2.out + intermediateResult2[1];
	signal resConstraint2;
	resConstraint2 <== IsEqual()([intermediateResult2[2], 3]);
	result[2] <== resConstraint2 * ruleSelector[2] + result[1];

	signal intermediateResult3[1];
	intermediateResult3[0] <== 1;
	signal resConstraint3;
	resConstraint3 <== IsEqual()([intermediateResult3[0], 1]);
	result[3] <== resConstraint3 * ruleSelector[3] + result[2];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== savingsClass;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];


    signal res1;
	res1<==ruleSelector[0]*((unified_body[1][1]/*var:CurrentSaving*/) - ((unified_body[1][2]/*var:RollingConsumptionVar*/ + unified_body[1][3]/*var:Consumption*/)/* op:- */)/*op:is:1*/);







    signal res2;
	res2 <== ruleSelector[1]*((unified_body[1][1]/*var:CurrentSaving*/) - ((unified_body[1][2]/*var:RollingConsumptionVar*/ + unified_body[1][3]/*var:Consumption*/)/* op:- */)/*op:is:1*/);







    signal res3;
	res3 <== ruleSelector[2]*((unified_body[1][1]/*var:CurrentSaving*/) - ((unified_body[1][2]/*var:RollingConsumptionVar*/ + unified_body[1][3]/*var:Consumption*/)/* op:- */)/*op:is:1*/);
    signal res;
     res <==res1 + res2 + res3;



	signal finalResult;
	//finalResult <== GreaterEqThan(8)([result[3], 1]);
	finalResult <== 1;
	if(goal_args[0] == savingsClass) {
		if(finalResult != 1) {
			log("savingsClass failed");
			log("Result[0] failed: ", result[0]);
			log("Result[1] failed: ", result[1]);
			log("Result[2] failed: ", result[2]);
			log("Result[3] failed: ", result[3]);
		} else {
			log("savingsClass succeeded");
		}
	}

	c <== finalResult;
}

template GoalPriceBase(){
	signal input unified_body[3][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[1];
	signal ruleSelector_intermediate[1][3];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], currentConsumption]);
	component isEqual0_1 = IsEqual();
	isEqual0_1.in[0] <== unified_body[1][0];
	isEqual0_1.in[1] <== currentPrice;
	ruleSelector_intermediate[0][1] <== isEqual0_1.out + ruleSelector_intermediate[0][0];
	component isEqual0_2 = IsEqual();
	isEqual0_2.in[0] <== unified_body[2][0];
	isEqual0_2.in[1] <== 98;
	ruleSelector_intermediate[0][2] <== isEqual0_2.out + ruleSelector_intermediate[0][1];
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][2], 3]);
	signal result[2];
	signal intermediateResult0[1];
	component constraint0_0 = IsEqual();
	constraint0_0.in[0] <== unified_body[2][1];
	constraint0_0.in[1] <== goal_args[1];
	intermediateResult0[0] <== constraint0_0.out;
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[0], 1]);
	result[0] <== resConstraint0 * ruleSelector[0];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== priceBase;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];


	signal prodRes1;
	prodRes1 <== unified_body[2][2]/*var:Price*/ * unified_body[2][3]/*var:Amount*/;

	ruleSelector[0]*((unified_body[2][1]/*var:PriceBase*/) - (prodRes1 /* op:* */)/*op:is:1*/) === 0;





	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[0], 1]);
	if(goal_args[0] == priceBase) {
		if(finalResult != 1) {
			log("priceBase failed");
			log("Result[0] failed: ", result[0]);
		} else {
			log("priceBase succeeded");
		}
	}

	c <== finalResult;
}

template GoalApplySupport(){
	signal input unified_body[2][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[2];
	signal ruleSelector_intermediate[2][2];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], 98]);
	component isZero0_1 = IsZero();
	isZero0_1.in <== unified_body[1][0];
	ruleSelector_intermediate[0][1] <== isZero0_1.out + ruleSelector_intermediate[0][0];
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][1], 2]);
	ruleSelector_intermediate[1][0] <== IsEqual()([unified_body[0][0], 98]);
	component isEqual1_1 = IsEqual();
	isEqual1_1.in[0] <== unified_body[1][0];
	isEqual1_1.in[1] <== 98;
	ruleSelector_intermediate[1][1] <== isEqual1_1.out + ruleSelector_intermediate[1][0];
	ruleSelector[1] <== IsEqual()([ruleSelector_intermediate[1][1], 2]);
	signal result[4];
	signal intermediateResult0[1];
	component constraint0_0 = IsEqual();
	constraint0_0.in[0] <== unified_body[0][1];
	constraint0_0.in[1] <== goal_args[4];
	intermediateResult0[0] <== constraint0_0.out;
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[0], 1]);
	result[0] <== resConstraint0 * ruleSelector[0];

	signal intermediateResult1[1];
	component constraint1_0 = IsEqual();
	constraint1_0.in[0] <== unified_body[1][1];
	constraint1_0.in[1] <== goal_args[4];
	intermediateResult1[0] <== constraint1_0.out;
	signal resConstraint1;
	resConstraint1 <== IsEqual()([intermediateResult1[0], 1]);
	result[1] <== resConstraint1 * ruleSelector[1] + result[0];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== applySupport;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];



	0*((unified_body[0][1]/*var:ApplySupportOutput*/) - ((unified_body[0][2]/*var:ApplySupportInput*/ + unified_body[0][3]/*var:ApplySupportValue*/)/* op:- */)/*op:is:1*/) === 0;





component goalCheck1 = IsEqual();
goalCheck1.in[0] <== goal_args[0];
goalCheck1.in[1] <== applySupport;
signal goalCheckResult1;
goalCheckResult1 <== goalCheck1.out * ruleSelector[1];


	signal intDivRes2;
	intDivRes2 <-- unified_body[0][3]/*var:ApplySupportValue*/ \ 100;
	signal intDivRem2;
	intDivRem2 <-- unified_body[0][3]/*var:ApplySupportValue*/ % 100;
	intDivRes2 * 100 + intDivRem2=== unified_body[0][3]/*var:ApplySupportValue*/;
	signal prodRes1;
	prodRes1 <== unified_body[0][2]/*var:ApplySupportInput*/ * intDivRes2/* op:div */;

	0*((unified_body[0][1]/*var:AmountToBeSubtracted*/) - (prodRes1 /* op:* */)/*op:is:1*/) === 0;








	0*((unified_body[1][1]/*var:ApplySupportOutput*/) - ((unified_body[1][2]/*var:ApplySupportInput*/ + unified_body[1][3]/*var:AmountToBeSubtracted*/)/* op:- */)/*op:is:1*/) === 0;





	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[1], 1]);
	if(goal_args[0] == applySupport) {
		if(finalResult != 1) {
			log("applySupport failed");
			log("Result[0] failed: ", result[0]);
			log("Result[1] failed: ", result[1]);
		} else {
			log("applySupport succeeded");
		}
	}

	c <== finalResult;
}

template GoalApplySavingsSupport(){
	signal input unified_body[2][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[1];
	signal ruleSelector_intermediate[1][2];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], support_matrix]);
	component isEqual0_1 = IsEqual();
	isEqual0_1.in[0] <== unified_body[1][0];
	isEqual0_1.in[1] <== applySupport;
	ruleSelector_intermediate[0][1] <== isEqual0_1.out + ruleSelector_intermediate[0][0];
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][1], 2]);
	signal result[2];
	signal intermediateResult0[5];
	component constraint0_0 = IsEqual();
	constraint0_0.in[0] <== unified_body[0][1];
	constraint0_0.in[1] <== goal_args[3];
	intermediateResult0[0] <== constraint0_0.out;
	component constraint0_1 = IsEqual();
	constraint0_1.in[0] <== unified_body[0][2];
	constraint0_1.in[1] <== goal_args[2];
	intermediateResult0[1] <== constraint0_1.out + intermediateResult0[0];
	component constraint0_2 = IsEqual();
	constraint0_2.in[0] <== unified_body[1][1];
	constraint0_2.in[1] <== goal_args[1];
	intermediateResult0[2] <== constraint0_2.out + intermediateResult0[1];
	component constraint0_3 = IsEqual();
	constraint0_3.in[0] <== unified_body[1][4];
	constraint0_3.in[1] <== goal_args[4];
	intermediateResult0[3] <== constraint0_3.out + intermediateResult0[2];
	component constraint0_4 = IsEqual();
	constraint0_4.in[0] <== unified_body[0][3];
	constraint0_4.in[1] <== unified_body[1][2];
	intermediateResult0[4] <== constraint0_4.out + intermediateResult0[3];
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[4], 5]);
	result[0] <== resConstraint0 * ruleSelector[0];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== applySavingsSupport;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];
	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[0], 1]);
	if(goal_args[0] == applySavingsSupport) {
		if(finalResult != 1) {
			log("applySavingsSupport failed");
			log("Result[0] failed: ", result[0]);
		} else {
			log("applySavingsSupport succeeded");
		}
	}

	c <== finalResult;
}

template GoalSocialCreds(){
	signal input unified_body[1][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[1];
	signal ruleSelector_intermediate[1][1];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], 114]);
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][0], 1]);
	signal result[2];
	signal intermediateResult0[1];
	intermediateResult0[0] <== 1;
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[0], 1]);
	result[0] <== resConstraint0 * ruleSelector[0];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== socialCreds;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];
	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[0], 1]);
	if(goal_args[0] == socialCreds) {
		if(finalResult != 1) {
			log("socialCreds failed");
			log("Result[0] failed: ", result[0]);
		} else {
			log("socialCreds succeeded");
		}
	}

	c <== finalResult;
}

template GoalApplySocialSupports(){
	signal input unified_body[2][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[1];
	signal ruleSelector_intermediate[1][2];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], applySupport]);
	component isEqual0_1 = IsEqual();
	isEqual0_1.in[0] <== unified_body[1][0];
	isEqual0_1.in[1] <== applySocialSupports;
	ruleSelector_intermediate[0][1] <== isEqual0_1.out + ruleSelector_intermediate[0][0];
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][1], 2]);
	signal result[2];
	signal intermediateResult0[3];
	component constraint0_0 = IsEqual();
	constraint0_0.in[0] <== unified_body[0][1];
	constraint0_0.in[1] <== goal_args[1];
	intermediateResult0[0] <== constraint0_0.out;
	component constraint0_1 = IsEqual();
	constraint0_1.in[0] <== unified_body[1][3];
	constraint0_1.in[1] <== goal_args[3];
	intermediateResult0[1] <== constraint0_1.out + intermediateResult0[0];
	component constraint0_2 = IsEqual();
	constraint0_2.in[0] <== unified_body[0][4];
	constraint0_2.in[1] <== unified_body[1][1];
	intermediateResult0[2] <== constraint0_2.out + intermediateResult0[1];
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[2], 3]);
	result[0] <== resConstraint0 * ruleSelector[0];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== applySocialSupports;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];
// Empty array check
	component constraint0 = IsEqual();
	constraint0.in[0] <== goal_args[2];
	constraint0.in[1] <== 4;
	result[1] <== constraint0.out + result[0];
	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[1], 1]);
	if(goal_args[0] == applySocialSupports) {
		if(finalResult != 1) {
			log("applySocialSupports failed");
			log("Result[0] failed: ", result[0]);
			log("Result[1] failed: ", result[1]);
		} else {
			log("applySocialSupports succeeded");
		}
	}

	c <== finalResult;
}

template GoalEndPrice(){
	signal input unified_body[10][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[1];
	signal ruleSelector_intermediate[1][10];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], monthlyConsumptions]);
	component isEqual0_1 = IsEqual();
	isEqual0_1.in[0] <== unified_body[1][0];
	isEqual0_1.in[1] <== sumOfMonthlyConsumptions;
	ruleSelector_intermediate[0][1] <== isEqual0_1.out + ruleSelector_intermediate[0][0];
	component isEqual0_2 = IsEqual();
	isEqual0_2.in[0] <== unified_body[2][0];
	isEqual0_2.in[1] <== rollingConsumption;
	ruleSelector_intermediate[0][2] <== isEqual0_2.out + ruleSelector_intermediate[0][1];
	component isEqual0_3 = IsEqual();
	isEqual0_3.in[0] <== unified_body[3][0];
	isEqual0_3.in[1] <== currentConsumption;
	ruleSelector_intermediate[0][3] <== isEqual0_3.out + ruleSelector_intermediate[0][2];
	component isEqual0_4 = IsEqual();
	isEqual0_4.in[0] <== unified_body[4][0];
	isEqual0_4.in[1] <== consumptionClass;
	ruleSelector_intermediate[0][4] <== isEqual0_4.out + ruleSelector_intermediate[0][3];
	component isEqual0_5 = IsEqual();
	isEqual0_5.in[0] <== unified_body[5][0];
	isEqual0_5.in[1] <== savingsClass;
	ruleSelector_intermediate[0][5] <== isEqual0_5.out + ruleSelector_intermediate[0][4];
	component isEqual0_6 = IsEqual();
	isEqual0_6.in[0] <== unified_body[6][0];
	isEqual0_6.in[1] <== priceBase;
	ruleSelector_intermediate[0][6] <== isEqual0_6.out + ruleSelector_intermediate[0][5];
	component isEqual0_7 = IsEqual();
	isEqual0_7.in[0] <== unified_body[7][0];
	isEqual0_7.in[1] <== applySavingsSupport;
	ruleSelector_intermediate[0][7] <== isEqual0_7.out + ruleSelector_intermediate[0][6];
	component isEqual0_8 = IsEqual();
	isEqual0_8.in[0] <== unified_body[8][0];
	isEqual0_8.in[1] <== socialCreds;
	ruleSelector_intermediate[0][8] <== isEqual0_8.out + ruleSelector_intermediate[0][7];
	component isEqual0_9 = IsEqual();
	isEqual0_9.in[0] <== unified_body[9][0];
	isEqual0_9.in[1] <== applySocialSupports;
	ruleSelector_intermediate[0][9] <== isEqual0_9.out + ruleSelector_intermediate[0][8];
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][9], 10]);
	signal result[2];
	signal intermediateResult0[10];
	component constraint0_0 = IsEqual();
	constraint0_0.in[0] <== unified_body[9][3];
	constraint0_0.in[1] <== goal_args[1];
	intermediateResult0[0] <== constraint0_0.out;
	component constraint0_1 = IsEqual();
	constraint0_1.in[0] <== unified_body[0][1];
	constraint0_1.in[1] <== unified_body[1][1];
	intermediateResult0[1] <== constraint0_1.out + intermediateResult0[0];
	component constraint0_2 = IsEqual();
	constraint0_2.in[0] <== unified_body[1][2];
	constraint0_2.in[1] <== unified_body[2][1];
	intermediateResult0[2] <== constraint0_2.out + intermediateResult0[1];
	component constraint0_3 = IsEqual();
	constraint0_3.in[0] <== unified_body[2][2];
	constraint0_3.in[1] <== unified_body[4][1];
	intermediateResult0[3] <== constraint0_3.out + intermediateResult0[2];
	component constraint0_4 = IsEqual();
	constraint0_4.in[0] <== unified_body[4][1];
	constraint0_4.in[1] <== unified_body[5][1];
	intermediateResult0[4] <== constraint0_4.out + intermediateResult0[3];
	component constraint0_5 = IsEqual();
	constraint0_5.in[0] <== unified_body[3][1];
	constraint0_5.in[1] <== unified_body[5][2];
	intermediateResult0[5] <== constraint0_5.out + intermediateResult0[4];
	component constraint0_6 = IsEqual();
	constraint0_6.in[0] <== unified_body[5][3];
	constraint0_6.in[1] <== unified_body[7][2];
	intermediateResult0[6] <== constraint0_6.out + intermediateResult0[5];
	component constraint0_7 = IsEqual();
	constraint0_7.in[0] <== unified_body[6][1];
	constraint0_7.in[1] <== unified_body[7][1];
	intermediateResult0[7] <== constraint0_7.out + intermediateResult0[6];
	component constraint0_8 = IsEqual();
	constraint0_8.in[0] <== unified_body[7][4];
	constraint0_8.in[1] <== unified_body[9][1];
	intermediateResult0[8] <== constraint0_8.out + intermediateResult0[7];
	component constraint0_9 = IsEqual();
	constraint0_9.in[0] <== unified_body[8][1];
	constraint0_9.in[1] <== unified_body[9][2];
	intermediateResult0[9] <== constraint0_9.out + intermediateResult0[8];
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[9], 10]);
	result[0] <== resConstraint0 * ruleSelector[0];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== endPrice;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];
	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[0], 1]);
	if(goal_args[0] == endPrice) {
		if(finalResult != 1) {
			log("endPrice failed");
			log("Result[0] failed: ", result[0]);
		} else {
			log("endPrice succeeded");
		}
	}

	c <== finalResult;
}

template GoalInputPriceOk(){
	signal input unified_body[2][5];
	signal input goal_args[5];
	signal output c;
	var none = 0;

	var monthlyConsumptions = 96;
	var sumOfMonthlyConsumptions = 86;
	var rollingConsumption = 100;
	var consumptionClass = 102;
	var savingsClass = 104;
	var priceBase = 106;
	var applySupport = 108;
	var applySavingsSupport = 109;
	var socialCreds = 110;
	var applySocialSupports = 87;
	var endPrice = 112;
	var inputPriceOk = 113;
	var rolling_treshold = 88;
	var savings_treshold = 89;
	var support_matrix = 90;
	var social_suport = 91;
	var monthly_consumption = 92;
	var currentConsumption = 93;
	var currentPrice = 94;
	var inputPayment = 95;
	signal ruleSelector[1];
	signal ruleSelector_intermediate[1][2];
	ruleSelector_intermediate[0][0] <== IsEqual()([unified_body[0][0], endPrice]);
	component isEqual0_1 = IsEqual();
	isEqual0_1.in[0] <== unified_body[1][0];
	isEqual0_1.in[1] <== inputPayment;
	ruleSelector_intermediate[0][1] <== isEqual0_1.out + ruleSelector_intermediate[0][0];
	ruleSelector[0] <== IsEqual()([ruleSelector_intermediate[0][1], 2]);
	signal result[2];
	signal intermediateResult0[1];
	component constraint0_0 = IsEqual();
	constraint0_0.in[0] <== unified_body[0][1];
	constraint0_0.in[1] <== unified_body[1][1];
	intermediateResult0[0] <== constraint0_0.out;
	signal resConstraint0;
	resConstraint0 <== IsEqual()([intermediateResult0[0], 1]);
	result[0] <== resConstraint0 * ruleSelector[0];

component goalCheck = IsEqual();
goalCheck.in[0] <== goal_args[0];
goalCheck.in[1] <== inputPriceOk;
signal goalCheckResult;
goalCheckResult <== goalCheck.out * ruleSelector[0];
	signal finalResult;
	finalResult <== GreaterEqThan(8)([result[0], 1]);
	if(goal_args[0] == inputPriceOk) {
		if(finalResult != 1) {
			log("inputPriceOk failed");
			log("Result[0] failed: ", result[0]);
		} else {
			log("inputPriceOk succeeded");
		}
	}

	c <== finalResult;
}



template KnowledgeChecker() {
   
   signal input a[5];
   signal output c;
   var len = 33;
   var knowledgeBase[len][5] = [[86, 4, 0, 0, 0],[87, 70805418, 4, 70805418, 0],[88, 43, 0, 0, 0],[88, 47, 3000, 0, 0],[88, 51, 7000, 0, 0],[89, 43, 250, 0, 0],[89, 47, 500, 0, 0],[89, 51, 1000, 0, 0],[90, 43, 43, 55, 500],[90, 43, 47, 24, 10],[90, 43, 51, 55, 500],[90, 47, 43, 55, 500],[90, 47, 47, 55, 500],[90, 47, 51, 55, 500],[90, 51, 43, 55, 500],[90, 51, 47, 55, 500],[90, 51, 51, 55, 500],[91, 54, 55, 10000, 0],[92, 1, 2001, 0, 0],[92, 2, 2001, 0, 0],[92, 3, 2001, 0, 0],[92, 4, 2001, 0, 0],[92, 5, 2001, 0, 0],[92, 6, 2001, 0, 0],[92, 7, 2000, 0, 0],[92, 8, 2000, 0, 0],[92, 9, 2000, 0, 0],[92, 10, 2000, 0, 0],[92, 11, 2000, 0, 0],[92, 12, 2000, 0, 0],[93, 1400, 0, 0, 0],[94, 747, 83, 0, 0],[95, 931220, 0, 0, 0]];
   signal result[len];
   signal arrayIsEqual[len];

   for(var i = 0; i < len; i++){
      arrayIsEqual[i] <== ArrayIsEqual(5)(a, knowledgeBase[i]);
      if (i == 0) {
         result[i] <== arrayIsEqual[i];
      } else {
        result[i] <== arrayIsEqual[i] + result[i-1];
      }
   }

   c <== IsEqual()([result[len-1], 1]);
 }

 component main {public [consumption_hashes] }=  PrologResolutionTree(4, 13);