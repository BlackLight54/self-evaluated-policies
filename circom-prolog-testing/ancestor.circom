pragma circom 2.0.0;


template CheckNode(){
   signal input unified_body[3][3];
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
      goal_ancestor.unified_body[2] <-- unified_body[2];
      goal_ancestor.goal_args <-- goal_args;
      result = goal_ancestor.c;
   }

   c <-- result;
   c === 1;
}

template GoalAncestor() {
   signal input unified_body[3][3];
   signal input goal_args[3];
   signal output c;
   var result=0;
   var parent = 4;
   var ancestor = 5;
   var none = 0;

   goal_args[0] === 5;
   component knowledge = KnowledgeChecker();
   if (unified_body[0][0] == parent && unified_body[1][0] == none && unified_body[2][0] == none){
      knowledge.a <-- unified_body[0];
      result = knowledge.c;
   } else if(unified_body[0][0] == 5 && unified_body[1][0] == 4 && unified_body[2][0] == 0){
    if(goal_args[0] == unified_body[0][0] &&
        unified_body[0][1] == unified_body[1][2] &&
        goal_args[1] == unified_body[1][2]
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

 component main = CheckNode();

