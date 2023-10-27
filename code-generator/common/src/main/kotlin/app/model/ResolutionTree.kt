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

    fun standardize(unificationCount_input: Int = 0, max_elements: Int = 0): ResolutionTree {
        val elementCount = if (max_elements == 0) getMaxElementCount() else max_elements
        val unificationCount = if (unificationCount_input != 0) unificationCount_input else getMaxUnficationCount()
        return standardizeElements(elementCount, unificationCount)
    }

    fun toBFSJson(totalElements: Int = 200): String {
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
        val childrenCount = mutableListOf<Int>()
        // Do the BFS
        val queue = mutableListOf<ResolutionTree>()
        queue.add(this)
        while (queue.isNotEmpty()) {
            val current = queue.removeAt(0)
            goals.add(current.goal)
            unifications.add(current.unification)
            childrenCount.add(current.children.size)
            queue.addAll(current.children)
        }

        while (goals.size < totalElements) {
            goals.add(IntArray(goals[0].size) { 0 }.toList())
            childrenCount.add(0)
            val mutableList = mutableListOf<List<Int>>()
            for (i in 0 until unifications[0].size) {
                mutableList.add(IntArray(unifications[0][i].size) { 0 }.toList())
            }
            unifications.add(mutableList)
        }

        val json = JSONObject()
        json.put("goals", goals)
        json.put("unifiedBodies", unifications)
        json.put("childCountArray", childrenCount)
        return json.toString()
    }

    private fun standardizeBranching(branchingFactor: Int, maxDepth: Int, depth: Int = 1): ResolutionTree {
        // If we are at the max depth, we don't need to do anything
        if (depth == maxDepth) return this

        val newChildren = mutableListOf<ResolutionTree>()
        children.forEach { newChildren.add(it.standardizeBranching(branchingFactor, maxDepth, depth + 1)) }
        while (newChildren.size < branchingFactor) {
            newChildren.add(
                ResolutionTree(listOf(), listOf(), listOf()).standardizeBranching(
                    branchingFactor,
                    maxDepth,
                    depth + 1
                )
            )
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
        children.forEach { newChildren.add(it.standardizeElements(elementCount, unificationCount)) }
        return ResolutionTree(newGoal, newUnification, newChildren)
    }

    fun getMaxDepth(): Int {
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
        val unificationCount = if (unification.isNotEmpty()) unification.maxOf { it.size } else 0
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

    fun getMaxUnficationCount(): Int {
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
        fun parseJson(json: String, mapping: Map<String, Int>): ResolutionTree {
            val jsonObject = try {
                JSONObject(json)
            } catch (e: JSONException) {
                JSONObject("{}")
            }
            if (jsonObject.has("goal")) {
                val goal = Parser.parseProlog(jsonObject.getString("goal") + ".")[0].head

                val unification = if ((jsonObject.getJSONObject("unification")
                        .get("body") is String).not()
                ) jsonObject.getJSONObject("unification").getJSONArray("body")
                    .map { Parser.parsePredicate(it.toString()).encode(mapping) } else listOf<List<Int>>()
                val children = jsonObject.getJSONArray("ztree").map { parseJson(it.toString(), mapping) }
                return ResolutionTree(goal.encode(mapping), unification, children)
            } else {
                // Its a leaf, i.e. either true or false
                val goal = listOf(mapping[json]!!)
                val unification = listOf<List<Int>>()
                val children = listOf<ResolutionTree>()
                return ResolutionTree(goal, unification, children)
            }
        }
    }

}