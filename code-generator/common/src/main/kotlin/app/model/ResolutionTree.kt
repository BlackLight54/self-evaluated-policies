package app.model

import hu.bme.app.Parser
import org.json.JSONException
import org.json.JSONObject

data class ResolutionTree(var goal: List<Int>, var unification: List<List<Int>>, val children: List<ResolutionTree>) {

    fun getMaxChildrenCount(): Int {
        if (children.isEmpty()) return 0
        var max = children.size
        for (child in children) {
            val count = child.getMaxChildrenCount()
            if (count > max) {
                max = count
            }
        }
        return max
    }

    fun standardize(branchingFactor_input: Int = 0,maxDepth_input: Int = 0, unificationCount_input: Int = 0): ResolutionTree {
        val elementCount = getMaxElementCount()
        val unificationCount = if(unificationCount_input != 0) unificationCount_input else getMaxUnficationCount()
        val maxDepth = if(maxDepth_input != 0) maxDepth_input else  getMaxDepth()
        val branchingFactor = if(branchingFactor_input != 0) branchingFactor_input else getMaxChildrenCount()
        return standardizeBranching(branchingFactor,maxDepth).standardizeElements(elementCount, unificationCount)
    }

    fun toBFSJson() : String{
        // Generate a BFS json
        // It should have two fields: goals and unifications
        // goals should be a list of lists
        // unifications should be a list of lists of lists
        // The first element of the goals list should be the root node
        // The first element of the unifications list should be the root node
        // The second element of the goals list should be the first child of the root node
        // The second element of the unifications list should be the first child of the root node
        // The third element of the goals list should be the second child of the root node
        // ... and so on
        val goals = mutableListOf<List<Int>>()
        val unifications = mutableListOf<List<List<Int>>>()
        // Do the BFS
        val queue = mutableListOf<ResolutionTree>()
        queue.add(this)
        while (queue.isNotEmpty()) {
            val current = queue.removeAt(0)
            goals.add(current.goal)
            unifications.add(current.unification)
            queue.addAll(current.children)
        }
        val json = JSONObject()
        json.put("goals",goals)
        json.put("unifiedBodies",unifications)
        return json.toString()
    }

    private fun standardizeBranching(branchingFactor: Int,maxDepth :Int, depth : Int = 1): ResolutionTree {
        // If we are at the max depth, we don't need to do anything
        if (depth == maxDepth) return this

        val newChildren = mutableListOf<ResolutionTree>()
        children.forEach { newChildren.add(it.standardizeBranching(branchingFactor,maxDepth,depth+1)) }
        while (newChildren.size < branchingFactor) {
            newChildren.add(ResolutionTree(listOf(), listOf(), listOf()).standardizeBranching(branchingFactor,maxDepth,depth+1))
        }
        return ResolutionTree(goal, unification, newChildren)
    }

    private fun standardizeElements(elementCount: Int, unificationCount: Int): ResolutionTree {
        val newGoal = mutableListOf<Int>()
        val newUnification = mutableListOf<MutableList<Int>>()
        goal.forEach { newGoal.add(it) }
        while (newGoal.size < elementCount) {
            newGoal.add(0)
        }
        unification.forEach { newUnification.add(it.toMutableList()) }
        while (newUnification.size < unificationCount) {
            newUnification.add(mutableListOf())
        }
        newUnification.forEach {
            while (it.size < elementCount) {
                it.add(0)
            }
        }
        val newChildren = mutableListOf<ResolutionTree>()
        children.forEach { newChildren.add(it.standardizeElements(elementCount,unificationCount)) }
        return ResolutionTree(newGoal, newUnification, newChildren)
    }

    fun getMaxDepth() : Int {
        if (children.isEmpty()) return 1
        var max = 0
        for (child in children) {
            val depth = child.getMaxDepth()
            if (depth > max) {
                max = depth
            }
        }
        return max + 1
    }

    fun getMaxElementCount(): Int {
        val goalCount = goal.size
        val unificationCount = if(unification.isNotEmpty()) unification.maxOf { it.size } else 0
        val maxHere = if (goalCount > unificationCount) goalCount else unificationCount
        if (children.isEmpty()) return maxHere
        var max = maxHere
        for (child in children) {
            val count = child.getMaxElementCount()
            if (count > max) {
                max = count
            }
        }
        return max
    }

    fun getMaxUnficationCount():Int {
        var max = unification.size
        if (children.isEmpty()) return max
        children.forEach {
            val count = it.getMaxUnficationCount()
            if (count > max) {
                max = count
            }
        }
        return max
    }

    companion object {
       /* fun parseJson(json: String, mapping: Map<String, Int>): ResolutionTree {
            val jsonObject = try {
                JSONObject(json)
            } catch (e: JSONException) {
                JSONObject("{}")
            }
            if (jsonObject.has("goal")) {
                val goal = Parser.parsePredicate(jsonObject.getString("goal")).encode(mapping)
                val unification = jsonObject.getJSONObject("unification").getJSONArray("body").map { Parser.parsePredicate(it.toString()).encode(mapping) }
                val children = jsonObject.getJSONArray("ztree").map { parseJson(it.toString(), mapping) }
                return ResolutionTree(goal, unification, children)
            } else {
                // Its a leaf, i.e. either true or false
                val goal = listOf(mapping[json]!!)
                val unification = listOf<List<Int>>()
                val children = listOf<ResolutionTree>()
                return ResolutionTree(goal, unification, children)
            }
        }*/
    }

}