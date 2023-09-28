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
    queue.push(tree);
    while (queue.length > 0) {
        let node = queue.shift();
        if (node == undefined)            {
            goalArr.push([0, 0, 0]);
            continue;
        }
        if (node.unification) {
            goalArr.push(parseCompositeNode(node));
        }
        if (node == 'true') {
            goalArr.push(parseTrueNode(node));
        }
        if (node.ztree) {
            for (let i = 0; i < maxParameterCount; i++) {
                let item = node.ztree[i];
                queue.push(item);
            }
        }
        
    }
    return goalArr;
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
    } catch (err) {
        console.log('Error parsing JSON:', err);
    }
});