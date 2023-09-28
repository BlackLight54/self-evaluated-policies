pragma circom 2.0.0;


template PrologResolutionTreeArray(totalNodes,maxUnification) {
   signal input goals[totalNodes][3]; 
   signal input unifiedBodies[totalNodes][maxUnification][3];
   signal output c;
   var parent = 4;
   var ancestor = 5;
   var true = 6;

   var unknown = 8;
   var result = 0;

   var node[3] = goals[0];
   var unifiedBody[maxUnification][3] = unifiedBodies[0];
   // The first node cannot be true
   assert(node[0] != true);

   component checkNode1 = CheckNode();
   checkNode1.unified_body <-- unifiedBody;
   checkNode1.goal_args <-- node;
   result += checkNode1.c;
   component transitionLogic1 = TransitionLogic();
   transitionLogic1.prevUnifiedBodies <-- unifiedBodies[0];
   transitionLogic1.currentGoal <-- goals[1];

   node = goals[1];
   unifiedBody = unifiedBodies[1];
   
   unifiedBody = unifiedBodies[1];
   component checkNode2 = CheckNode();
   checkNode2.unified_body <-- unifiedBody;
   checkNode2.goal_args <-- node;
   result += checkNode2.c;
   component transitionLogic2 = TransitionLogic();
   transitionLogic2.prevUnifiedBodies <-- unifiedBodies[1];
   transitionLogic2.currentGoal <-- goals[2];


   node = goals[2];
   unifiedBody = unifiedBodies[2];
   component checkNode3 = CheckNode();
   checkNode3.unified_body <-- unifiedBody;
   checkNode3.goal_args <-- node;
   result += checkNode3.c;
   component transitionLogic3 = TransitionLogic();
   transitionLogic3.prevUnifiedBodies <-- unifiedBodies[2];
   transitionLogic3.currentGoal <-- goals[3];


   node = goals[3];
   unifiedBody = unifiedBodies[3];
   component checkNode4 = CheckNode();
   checkNode4.unified_body <-- unifiedBody;
   checkNode4.goal_args <-- node;
   result += checkNode4.c;
   component transitionLogic4 = TransitionLogic();
   transitionLogic4.prevUnifiedBodies <-- unifiedBodies[3];
   transitionLogic4.currentGoal <-- goals[4];


   node = goals[4];
   unifiedBody = unifiedBodies[4];
   component checkNode5 = CheckNode();
   checkNode5.unified_body <-- unifiedBody;
   checkNode5.goal_args <-- node;
   result += checkNode5.c;
   component transitionLogic5 = TransitionLogic();
   transitionLogic5.prevUnifiedBodies <-- unifiedBodies[4];
   transitionLogic5.currentGoal <-- goals[5];

   node = goals[5];
   unifiedBody = unifiedBodies[5];
   component checkNode6 = CheckNode();
   checkNode6.unified_body <-- unifiedBody;
   checkNode6.goal_args <-- node;
   result += checkNode6.c;



   // verify that all nodes are true
   c <-- result;
   c === totalNodes;
}

template TransitionLogic() {
   signal input prevUnifiedBodies[2][3];
   signal input currentGoal[3];
   signal output transition_okay;

   var ancestor = 5;
   var parent = 4;
   var true = 6;

   var result = 0;

   if(prevUnifiedBodies[0][0] == ancestor || prevUnifiedBodies[1][0] == ancestor) {
       if(currentGoal[0] == ancestor && (prevUnifiedBodies[0][1] == currentGoal[1] || prevUnifiedBodies[0][1] >= 8 || prevUnifiedBodies[1][1] == currentGoal[1] || prevUnifiedBodies[1][1] >= 8)) {
           result = 1;
       }
   }
   if(prevUnifiedBodies[0][0] == parent || prevUnifiedBodies[1][0] == parent) {
       if(currentGoal[0] == parent && (prevUnifiedBodies[0][1] == currentGoal[1] || prevUnifiedBodies[0][1] >= 8 || prevUnifiedBodies[1][1] == currentGoal[1] || prevUnifiedBodies[1][1] >= 8)) {
           result = 1;
       }
   }
   if((prevUnifiedBodies[0][0] == true && prevUnifiedBodies[0][1] == 0) || currentGoal[0] == true) {
       result = 1;
   }
   transition_okay <-- result;
   transition_okay === 1;
}


template CheckNode(){
   signal input unified_body[2][3];
   signal input goal_args[3];
   signal output c;
   var parent = 4;
   var ancestor = 5;
   var none = 0;
   var result = 0;
   component goal_ancestor = GoalAncestor();
   component knowledge = KnowledgeChecker();
   if(goal_args[0] == parent){
      knowledge.a <-- goal_args;
      result = knowledge.c;
   } else if(goal_args[0] == ancestor){
      goal_ancestor.unified_body[0] <-- unified_body[0];
      goal_ancestor.unified_body[1] <-- unified_body[1];
      goal_ancestor.goal_args <-- goal_args;
      result = goal_ancestor.c;
   } else if(goal_args[0] == 6 && goal_args[1] == 0 && goal_args[2] == 0){
      // "true" goal
      result = 1;
   }

   c <-- result;
   c === 1;
}

template GoalAncestor() {
   signal input unified_body[2][3];
   signal input goal_args[3];
   signal output c;
   var result=0;
   var parent = 4;
   var ancestor = 5;
   var none = 0;

   goal_args[0] === 5;
   component knowledge = KnowledgeChecker();
   if (unified_body[0][0] == parent && unified_body[1][0] == none){
      knowledge.a <-- unified_body[0];
      result = knowledge.c;
   } else if(unified_body[0][0] == ancestor && unified_body[1][0] == parent){
    if(goal_args[0] == unified_body[0][0] &&
        unified_body[0][1] == unified_body[1][2] &&
        goal_args[1] == unified_body[1][1]
      ){
         result=1;
      }
   } 
   
   c <-- result;
   c === 1;
   
}

template KnowledgeChecker() {
   
   signal input a[3];
   signal output c;
   var len = 2;
   var knowledgeBase[len][3] = [[4,1,2],[4,2,3]];
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

 component main = PrologResolutionTreeArray(6,2);

