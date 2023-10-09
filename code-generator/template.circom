pragma circom 2.0.0;

template TreeNode(branchFactor) {
   // Each node's data
   signal input goal[3];
   signal input unified_body[branchFactor][3];

    // If it's not a leaf, we connect the children
   signal input children_goals[branchFactor][3];
   signal output c;
    
   component transitiontochild[branchFactor];
   var result = 1;
   for(var i =0; i < branchFactor; i++) {
      log("Visiting child:", i, "of", branchFactor, "with goal", children_goals[i][0], children_goals[i][1], children_goals[i][2], "from parent", goal[0], goal[1], goal[2]);
      transitiontochild[i] = TransitionLogic();
      transitiontochild[i].prevUnifiedBodies <-- unified_body;
      transitiontochild[i].currentGoal <-- children_goals[i];
      result = result && transitiontochild[i].transition_okay;
   }
	//if(goal[0] == 0 && (children_goals[0][0] != 0 || children_goals[1][0] != 0)) {
	if(goal[0] == 0) {
		for(var i = 0; i < branchFactor; i++) {
			if(children_goals[i][0] != 0){
				result = 0;
			}
		}
	}

   component checknode = CheckNode();
   checknode.goal_args <-- goal;
   checknode.unified_body <-- unified_body;
   result = result && checknode.c;


   c <-- result;
   c === 1;
}

template PrologResolutionTree(depth, branchFactor) {
    // Calculate total nodes using geometric series sum formula
    var totalNodes = (branchFactor**(depth) - 1) / (branchFactor - 1);

    // Define the tree structure
    signal input goals[totalNodes][3];
    signal input unifiedBodies[totalNodes][branchFactor][3]; 
    signal output c;
    component nodes[totalNodes];
    for(var i = 0; i < totalNodes; i++) {
        nodes[i] = TreeNode(branchFactor);
    }
    
    

    for(var i = 0; i < totalNodes; i++){
         nodes[i].goal <-- goals[i];
         nodes[i].unified_body <-- unifiedBodies[i];

        for(var j = 0; j < branchFactor; j++) {
         var childGoalIndex = i * branchFactor + j+1;
      	if(childGoalIndex < totalNodes) {
         	nodes[i].children_goals[j] <-- goals[childGoalIndex];
         } else {
            nodes[i].children_goals[j] <-- [0,0,0];
         }
        }
    }
}
template TransitionLogic() {
   signal input prevUnifiedBodies[REPLACE_BRANCH_FACTOR][3];
   signal input currentGoal[3];
   signal output transition_okay;

REPLACE_PREDICATE_MAPPINGS
   var result = 0;

REPLACE_TRANSITION_RULES
   if((prevUnifiedBodies[0][0] == true && prevUnifiedBodies[0][1] == 0) || currentGoal[0] == true) {
       result = 1;
   }
	if(currentGoal[0] == 0) {
		result = 1;
	}

   if(result != 1){
      log("Failed transition:", currentGoal[0], currentGoal[1], currentGoal[2]);
   }

   transition_okay <-- result;
   transition_okay === 1;
}


template CheckNode(){
   signal input unified_body[REPLACE_BRANCH_FACTOR][3];
   signal input goal_args[3];
   signal output c;
REPLACE_PREDICATE_MAPPINGS
   var none = 0;
   var result = 0;
REPLACE_RULE_CALLS else if(goal_args[0] == true && goal_args[1] == 0 && goal_args[2] == 0){
      // "true" goal
      result = 1;
   } else if(goal_args[0] == 0 && goal_args[1] == 0 && goal_args[2] == 0){
      // "none" goal
      result = 1;
   }
   c <-- result;
   c === 1;
}

REPLACE_RULE_TEMPLATES

template KnowledgeChecker() {
   
   signal input a[3];
   signal output c;
   var len = REPLACE_KNOWLEDGE_BASE_LEN;
   var knowledgeBase[len][3] = [REPLACE_KNOWLEDGE_BASE_ARRAY];
   var result = 0;
   if(a[0] == 0 && a[1] == 0 && a[2] == 0) {
      result = 1;
   }
   for(var i = 0; i < len; i++){
      var so_far_so_good = 1;
      for(var j=0; j < 3; j++){
         if(a[j] != knowledgeBase[i][j]){
            so_far_so_good = 0;
         }
      }
      if(so_far_so_good == 1){
         result = 1;
      }
   }
   c <-- result;
   c === 1;
 }

 component main = PrologResolutionTree(REPLACE_MAX_DEPTH, REPLACE_BRANCH_FACTOR);
