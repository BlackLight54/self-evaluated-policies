const fs = require('fs');

const prologEncoding = {
    'anne': 1,
    'bob': 2,
    'carol': 3,
    'parent': 4,
    'ancestor': 5,
    'true': 6,
    'false': 7
}

const maxParameterCount = 2;

const maxUnificationCount = 2;

function parseClause(clause) {
    let predicate = String(clause).split('(')[0];
    let parameters = [];
    if (String(clause).split('(')[1] != undefined) {
        parameters = String(clause).split('(')[1].split(')')[0].split(',');
    }
    return { predicate: predicate, parameters: parameters };
}

function parseCompositeNode(node) {
    if (node.unification) {
        let goalClause = parseClause(node.unification.goal);
        let goalArr = [prologEncoding[goalClause.predicate]];
        
        for (let parameter of goalClause.parameters) {
            goalArr.push(prologEncoding[parameter]);
        }

        let unificationArr = [];
        if (node.ztree[0] === 'true') {
            unificationArr.push([prologEncoding['true'], 0, 0]);
        } else {
            for(let unifiedGoal of node.ztree){
                let unifiedGoalClause = parseClause(unifiedGoal.unification.goal);
                let unifiedGoalArr = [prologEncoding[unifiedGoalClause.predicate]];
                for (let parameter of unifiedGoalClause.parameters) {
                    unifiedGoalArr.push(prologEncoding[parameter]);
                }
                unificationArr.push(unifiedGoalArr);
            }
        }


        return { goalArr: goalArr, unificationArr: unificationArr };
    }
}

function parseTrueNode(node) {
    if (node == 'true') {
        return [prologEncoding['true'], 0, 0];

    }
}

function parseTreeBFS(tree) {
    let goalArr = [];
    let unificationArr = [];
    let queue = [];
    let levelQueue = [];  // To keep track of node levels
    let currentLevel = 0;
    let maxDepth = findMaxDepth(tree);

    queue.push(tree);
    levelQueue.push(currentLevel);

    while (queue.length > 0) {
        let node = queue.shift();
        currentLevel = levelQueue.shift();

        if (node === undefined || node === null) {
            goalArr.push([0, 0, 0]);
            unificationArr.push([[0, 0, 0]]);

            // Only push child placeholders if current level isn't the last one
            if (currentLevel < maxDepth - 1) {
                for (let i = 0; i < maxUnificationCount; i++) {
                    queue.push(undefined);
                    levelQueue.push(currentLevel + 1);
                }
            }
            continue;
        }

        if (node.unification) {
            let parsedNode = parseCompositeNode(node);
            goalArr.push(parsedNode.goalArr);
            unificationArr.push(parsedNode.unificationArr);
        }

        if (node === 'true') {
            goalArr.push(parseTrueNode(node));
            unificationArr.push([[0, 0, 0]]);
        }

        // Check if node has ztree property and push children
        if (node.ztree) {
            for (let i = 0; i < maxParameterCount; i++) {
                queue.push(node.ztree[i]);
                levelQueue.push(currentLevel + 1);
            }
        } else {
            // If node doesn't have ztree, still push placeholders for child nodes up to max depth
            if (currentLevel < maxDepth - 1) {
                for (let i = 0; i < maxParameterCount; i++) {
                    queue.push(undefined);
                    levelQueue.push(currentLevel + 1);
                }
            }
        }
    }
    return { goalArr, unificationArr };
}

function findMaxDepth(node) {
    if (node === undefined || node === null) {
        return 0;
    }
    let leftDepth = node.ztree && node.ztree[0] ? findMaxDepth(node.ztree[0]) : 0;
    let rightDepth = node.ztree && node.ztree[1] ? findMaxDepth(node.ztree[1]) : 0;

    return 1 + Math.max(leftDepth, rightDepth);
}




// Read the JSON file
fs.readFile('tree.json', 'utf8', (err, jsonString) => {
    if (err) {
        console.log("Error reading the file:", err);
        return;
    }

    try {
        const jsonObject = JSON.parse(jsonString);
        let { goalArr, unificationArr } = parseTreeBFS(jsonObject);
        console.log(goalArr);
        console.log('Length:', goalArr.length);
        console.log(unificationArr);
        console.log('Length:', unificationArr.length);
    } catch (err) {
        console.log('Error parsing JSON:', err);
    }
});