pragma circom 2.0.0;

template TreeNode(branchFactor) {
   // Each node's data
   signal input goal[MAX_BODY_SIZE];
   signal input unified_body[branchFactor][MAX_BODY_SIZE];

    // If it's not a leaf, we connect the children
   signal input children_goals[branchFactor][MAX_BODY_SIZE];
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
    // Define the maximum number of tree nodes
    var totalNodes = 200;

    // Define the tree structure
    signal input goals[totalNodes][MAX_BODY_SIZE];
    signal input unifiedBodies[totalNodes][branchFactor][MAX_BODY_SIZE]; 
    signal input childCountArray[totalNodes]; // This is the number of children each node has

    signal output c;
    component nodes[totalNodes];
    for(var i = 0; i < totalNodes; i++) {
        nodes[i] = TreeNode(branchFactor);
    }
    
    // Traverse the tree
    var currentIndex = 0; // Start from the root
    for(var i = 0; i < totalNodes; i++) {
        // Assign node values here
        // For example: nodes[i].goal <== goals[currentIndex];

        // Get number of children for the current node
        var numChildren = childCountArray[i];
        nodes[i].goal <-- goals[i];
        nodes[i].unified_body <-- unifiedBodies[i];

		var sum = 0;
		for(var j = 0; j < i;j++) {
			sum += childCountArray[j];
		}
        // Traverse through children
        for(var j = 0; j < branchFactor; j++) {
         var childGoal[MAX_BODY_SIZE];
			var childIndex = sum + j + 1;
			if(j < numChildren && childIndex < totalNodes) {
				childGoal = goals[childIndex];
			} else {
            	childGoal = [SUCH_EMPTY];
			}
            nodes[i].children_goals[j] <-- childGoal;
        }
    }


}

template TransitionLogic() {
   signal input prevUnifiedBodies[REPLACE_BRANCH_FACTOR][MAX_BODY_SIZE];
   signal input currentGoal[MAX_BODY_SIZE];
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
   signal input unified_body[REPLACE_BRANCH_FACTOR][MAX_BODY_SIZE];
   signal input goal_args[MAX_BODY_SIZE];
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
   
   signal input a[MAX_BODY_SIZE];
   signal output c;
   var len = REPLACE_KNOWLEDGE_BASE_LEN;
   var knowledgeBase[len][MAX_BODY_SIZE] = [REPLACE_KNOWLEDGE_BASE_ARRAY];
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
