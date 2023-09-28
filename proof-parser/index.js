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

function parseCompositeNode(node) {
    if (node.unification) {
        // console.log('Unification Goal:', node.unification.goal);
        // console.log('Unification Body:', node.unification.body.join(', '));
        // console.log('Goal:', node.goal);        
        let goalPredicate = String(node.goal).split('(')[0];
        let parameters = String(node.goal).split('(')[1].split(')')[0].split(',');
        // console.log('Goal Predicate:', goalPredicate);
        // console.log('Encoded Goal:', prologEncoding[goalPredicate]);
        // console.log('Parameters:', parameters);
        let thisGoalArr = [prologEncoding[goalPredicate]];
        for (let parameter of parameters) {
            thisGoalArr.push(prologEncoding[parameter]);
        }
        return thisGoalArr;
    }
}

function parseTrueNode(node) {
    if (node == 'true') {
        return [prologEncoding['true'], 0, 0];

    }
}
// function parseNode(node) {
//     // console.log('Goal:', node.goal);
//     let thisGoalArr = [];
//     if (node.unification) {
//         thisGoalArr = parseCompositeNode(node);
//     }

//     if (node.ztree) {
//         // console.log('ZTree:');
//         for (let item of node.ztree) {
//             if (typeof item === 'string') {
//                 // console.log('   ', [prologEncoding[item],0,0]);
//                 goalArr.push([prologEncoding[item], 0, 0]);
//                 // goalArr = goalArr.push([prologEncoding[item],0,0]);
//             } else {
//                 goalArr = goalArr.concat(parseNode(item));
//             }
//         }
//     }

//     return goalArr;

// }

function parseTreeBFS(tree) {
    let goalArr = [];
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

            // Only push child placeholders if current level isn't the last one
            if (currentLevel < maxDepth - 1) {
                for (let i = 0; i < maxParameterCount; i++) {
                    queue.push(undefined);
                    levelQueue.push(currentLevel + 1);
                }
            }
            continue;
        }

        if (node.unification) {
            goalArr.push(parseCompositeNode(node));
        }

        if (node === 'true') {
            goalArr.push(parseTrueNode(node));
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
    return goalArr;
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
        let goalArr = parseTreeBFS(jsonObject);
        console.log(goalArr);
        // console.log('Length:', goalArr.length);
    } catch (err) {
        console.log('Error parsing JSON:', err);
    }
});