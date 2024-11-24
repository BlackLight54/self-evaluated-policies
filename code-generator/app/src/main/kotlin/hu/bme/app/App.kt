/*
 * This Kotlin source file was generated by the Gradle 'init' task.
 */
package hu.bme.app

import app.model.ResolutionTree
import java.io.File
import java.util.*

const val maxPredicateSize = 15;
val  ARITHMETIC_OPERATIONS = listOf("is", "=", "<", ">");
const val DEBUGGING = true;
fun addGoalHeader() {

}

fun main() {
    val policyPrologProgram = File("../../policies/prolog/policy.pl").readText()

    val clauses = Parser.parseProlog(policyPrologProgram)
    //clauses.forEach { println(it) }
    val mapping = createMapping(clauses)

    mapping.forEach { (name, index) ->
        println("$name: $index")
    }
    println("Knowledge base:")
    clauses
        .filter { it.body.isEmpty() }
        .forEach { clause ->
        println(clause.head.encode(mapping))
    }
    val knowledgeBase = clauses.filter { it.body.isEmpty() }
    println("Rules:")
    val rules = clauses.filter { it.body.isNotEmpty() }.groupBy { it.head.name }
    rules.forEach { (name, rules) ->
        println("$name:")
        rules.forEach { rule ->
            println(
                "  ${
                    rule.body.joinToString(" & ") {
                        it.encode(mapping).toString()
                    }
                } => ${rule.head.encode(mapping)}"
            )
        }
    }


    val bucketVariables = findArrayPredicateVariables(rules)
    val maxBucketElementSize = findArrayTermMaxSize(rules)
    println("Bucket max size: $maxBucketElementSize")

    val generated_rule_code = StringBuilder()
    val maxPerdicateLength = rules.maxOf { it.value[0].body.maxOf { it.terms.size } } + 1
    rules.forEach { (name, rule_clauses) ->
        // Find matching terms in the body and the head of the rules
        // For example, if we have the following rule:
        // ancestor(X, Y) :- parent(X, Y).
        // Then the matching terms are:
        // X: goal first argument and first rule first argument
        // Y: goal second argument and first rule second argument
        // We need to find the matching terms because we need to unify them

        // Find matching terms in the body of the rules

        val resultsLine = "\tsignal result[${rule_clauses.size}];\n"


        val maxUniBody = rule_clauses.maxOf { it.body.size }
        generated_rule_code.appendLine(
            buildString {
                addGoalHeader(name, maxUniBody, maxPerdicateLength, this)
                clauses.groupBy { it.head.name }.forEach { (name, _) ->
                    appendLine("\tvar ${name} = ${mapping[name]};")
                }
                //appendLine("\n\tgoal_args[0] === ${name};")
                // Number of times we will name to check the knowledge base
                val knowledgeBaseUsage = initializeKnowledgeChecker(rule_clauses, knowledgeBase, this)
                val maxBodySize = rule_clauses.maxOf { it.body.size };

                appendLine("\tsignal ruleSelector[${rule_clauses.size}];")
                appendLine("\tsignal ruleSelector_intermediate[${rule_clauses.size}][${maxBodySize}];")

                var knowledgeUsageCounter = 0
                generateRuleSelectors(
                    rule_clauses,
                    mapping,
                    maxBodySize,
                    knowledgeUsageCounter,
                    this
                )


                val extraConstraintsCount = arrayConstraintCount(rule_clauses)
                generateConstraints(rule_clauses, this, extraConstraintsCount)

                rule_clauses.forEachIndexed { ind, rule ->
                    if (rule.body.any { it.hasAritmetic() }) {
                        appendLine(clauseGenerateArithmeticsCheck(rule,ind))
                    }
                    val knowledgeBody =
                        knowledgeBase.groupBy { it.head.name }.count { rule.body.map { it.name }.contains(it.key) }
                    if (rule.body.size == knowledgeBody) {
                        if (knowledgeBaseUsage == 1) {
                            appendLine("\t\tknowledge.a <-- unified_body[0];")
                            appendLine("\t\tresult = knowledge.c;")
                        } else {
                            appendLine("\tresult = 1;")
                            rule.body.forEachIndexed { index, predicate ->
                                appendLine("\tknowledge[$knowledgeUsageCounter].a <-- unified_body[$index];")
                                appendLine("\tresult = result && knowledge[$knowledgeUsageCounter].c;")
                                knowledgeUsageCounter++
                            }
                        }
                    } else if (rule.body[0].name == "=") {

                    } else {
                        //appendLine(constraints[ind])
                    }
                }
                var extraConstraintIndex = 0
                // Empty Array check
                if (rule_clauses.any { it.head.hasArray() }) {
                    appendLine("// Empty array check")
                    val arrayPositions = rule_clauses.map { clause ->
                        clause.head.terms.mapIndexed { index, term ->
                            if (term is Predicate && term.name == "[]") {
                                index
                            } else {
                                -1
                            }
                        }.filter { it != -1 }
                    }
                    arrayPositions[0].forEach {
                        appendLine("\tcomponent constraint${extraConstraintIndex} = IsEqual();")
                        appendLine("\tconstraint${extraConstraintIndex}.in[0] <== goal_args[${it + 1}];")
                        appendLine("\tconstraint${extraConstraintIndex}.in[1] <== ${mapping["[]"]};")
                        appendLine("\tresult[${rule_clauses.size + extraConstraintIndex}] <== constraint${extraConstraintIndex}.out + result[${rule_clauses.size + extraConstraintIndex - 1}];")
                        extraConstraintIndex++
                    }
                    /* val arrayPositions = rule_clauses.map { clause ->
                         clause.head.terms.mapIndexed { index, term ->
                             if (term is Predicate && term.name == "[]") {
                                 index
                             } else {
                                 -1
                             }
                         }.filter { it != -1 }
                     }
                     append("$rule_prefix if ( ")
                     var internalPrefix = ""
                     arrayPositions[0].forEach {
                         append("${internalPrefix}goal_args[${it + 1}] == ${mapping["[]"]}")
                         internalPrefix = " && "
                     }
                     append(" && unified_body[0][0] == ${mapping["true"]}")
                     append(" ) {")
                     appendLine("\n\t\tresult = 1;")
                     append("\t}")*/


                }
                val extraConstraintCountIfNeeded = if (rule_clauses.any { it.head.hasArray()}) 1 else 0
                appendLine("\tsignal finalResult;")
                appendLine("\tfinalResult <== GreaterEqThan(8)([result[${rule_clauses.size +extraConstraintCountIfNeeded- 1}], 1]);")
                if(DEBUGGING){
                    appendLine("\tif(finalResult != 1 && goal_args[0] == $name) {")
                    appendLine("\t\tlog(\"${name} failed\");")
                    IntArray(rule_clauses.size +extraConstraintCountIfNeeded) { it }.forEachIndexed { index, it ->
                        appendLine("\t\tlog(\"Result[${index}] failed: \", result[${it}]);")
                    }
                    appendLine("\t} else {")
                    appendLine("\t\tlog(\"${name} succeeded\");")
                    appendLine("\t}")
                }
                appendLine("\n\tc <== finalResult;")

                appendLine("}")
            }
        )

    } // rules.forEach
    val mapping_code = buildString {
        clauses.groupBy { it.head.name }.forEach { (name, _) ->
            appendLine("\tvar ${name} = ${mapping[name]};")
        }
        appendLine("\tvar true = ${mapping["true"]};")
    }

    val rule_calls = buildString {
        rules.forEach { (name, rules) ->
            appendLine(
                "\tcomponent ${name}Goal = Goal${
                    name.replaceFirstChar {
                        if (it.isLowerCase()) it.titlecase(
                            Locale.getDefault()
                        ) else it.toString()
                    }
                }();"
            )
        }
        val knowledgeAble = knowledgeBase.groupBy { it.head.name }
        if (knowledgeAble.size == 1) {
            appendLine("\tcomponent knowledge = KnowledgeChecker();")
        } else if (knowledgeAble.size > 1) {
            appendLine("\tcomponent knowledge[${knowledgeAble.size}];")
            appendLine("\tfor (var i = 0; i < ${knowledgeAble.size}; i++) {")
            appendLine("\t\tknowledge[i] = KnowledgeChecker();")
            appendLine("\t}")
        }
        var knowledgeUsageCounter = 0
        var rule_prefix = "\t"
        val arithmetics = ARITHMETIC_OPERATIONS.filter { mapping.contains(it) }
        val elementCount = rules.size + knowledgeAble.size + 2 + arithmetics.size
        appendLine("\tsignal result[${elementCount}];")
        appendLine("\tsignal ruleSelector[${elementCount}];")
        var ruleIndex = 0
        rules.forEach { (name, rule_clauses) ->
            val maxUniBody = rule_clauses.maxOf { it.body.size }
            appendLine("\truleSelector[${ruleIndex}] <== IsEqual()([goal_args[0], ${name}]);")
            appendLine("\t${name}Goal.goal_args <== goal_args;")
            for (i in 0..<maxUniBody) {
                appendLine("\t${name}Goal.unified_body[${i}] <== unified_body[${i}];")
            }
            if (ruleIndex == 0) {
                appendLine("\tresult[0] <== ${name}Goal.c*ruleSelector[0];")
            } else {
                appendLine("\tresult[$ruleIndex] <== ${name}Goal.c*ruleSelector[$ruleIndex] + result[${ruleIndex - 1}];")
            }
            ruleIndex++
        }
        knowledgeAble.forEach { (name, rules) ->
            if (knowledgeAble.size == 1) {
                appendLine("\truleSelector[${ruleIndex}] <== IsEqual()([goal_args[0], ${name}]);")
                appendLine("\tknowledge.a <== goal_args;")
                if (ruleIndex == 0) {
                    appendLine("\t\tresult[0] <== knowledge.c*ruleSelector[0];")
                } else {
                    appendLine("\t\tresult[$ruleIndex] <== knowledge.c*ruleSelector[$ruleIndex] + result[${ruleIndex - 1}];")
                }
            } else {

                appendLine("\truleSelector[${ruleIndex}] <== IsEqual()([goal_args[0], ${name}]);")
                appendLine("\tknowledge[$knowledgeUsageCounter].a <== goal_args;")
                if (ruleIndex == 0) {
                    appendLine("\tresult[0] <== knowledge[$knowledgeUsageCounter].c*ruleSelector[0];")
                } else {
                    appendLine("\tresult[$ruleIndex] <== knowledge[$knowledgeUsageCounter].c*ruleSelector[$ruleIndex] + result[${ruleIndex - 1}];")
                }
                knowledgeUsageCounter++

            }
            ruleIndex++
        }

        arithmetics.forEach {
            appendLine("\truleSelector[${ruleIndex}] <== IsEqual()([goal_args[0], ${mapping[it]}]);")
            appendLine("\tresult[$ruleIndex] <== ruleSelector[$ruleIndex] + result[${ruleIndex - 1}];")
            ruleIndex++
        }
    }
    val branchingFactor = 13//rules.maxOf { it.value[0].body.size }


    val transition_constraints = buildString {
        clauses.groupBy { it.head.name }.forEach { (name, clauses) ->

            var prefix = ""
            val prevBodyChecks = buildString {
                clauses.forEach { clause ->
                    var innerPrefix = ""
                    append(prefix)
                    clause.body.forEachIndexed { index, predicate ->
                        append(
                            "${innerPrefix}prevUnifiedBodies[${index}][0] == ${
                                if (
                                    Parser.SPECIAL_TERMS.any {
                                        predicate.name.contains(it)
                                    }.not() &&
                                    Parser.OPERATORS.any {
                                        predicate.name.contains(it)
                                    }.not()
                                )
                                    predicate.name else if (predicate.name == "=") mapping["true"] else mapping[predicate.name]
                            }"
                        )
                        innerPrefix = " && "
                    }
                    if (clause.body.isEmpty())
                        innerPrefix = ""
                    for (i in clause.body.size until branchingFactor) {
                        append("${innerPrefix}prevUnifiedBodies[${i}][0] == 0")
                        innerPrefix = " && "
                    }
                    prefix = " || "
                }
            }
            appendLine(
                "\tif(currentGoal[0] == $name) {\n"
                        + "\t\tif ( $prevBodyChecks ) {\n"
                        + "\t\t\tresult = 1;\n"
                        + "\t\t}\n"
                        + "\t}"
            )

        }
    }

    val maxDepth = 4;


    val template = File("../template.circom").readText()

    // Padded knowledge base. Each element in the knowledge base shall have a uniform length
    // Specifically, the length of the element with the longest length
    val knowLedgeBasePadded = knowledgeBase.map { clause ->
        val padded = clause.head.encode(mapping).toMutableList()
        while (padded.size < maxPerdicateLength) {
            padded.add(0)
        }
        padded
    }

    val bucketSize: Int = bucketVariables.values.sum()


    val generatedCode = template
        .replace("REPLACE_RULE_TEMPLATES", generated_rule_code.toString())
        .replace("REPLACE_PREDICATE_MAPPINGS", mapping_code)
        .replace("REPLACE_RULE_CALLS", rule_calls)
        .replace("REPLACE_KNOWLEDGE_BASE_LEN", knowledgeBase.size.toString())
        .replace("REPLACE_KNOWLEDGE_BASE_ARRAY", knowLedgeBasePadded.joinToString(",") { it.toString() })
        .replace("REPLACE_TRANSITION_RULES", transition_constraints)
        .replace("REPLACE_MAX_DEPTH", maxDepth.toString())
        .replace("REPLACE_BRANCH_FACTOR", branchingFactor.toString())
        .replace("MAX_BODY_SIZE", "$maxPerdicateLength")
        .replace("SUCH_EMPTY", IntArray(maxPerdicateLength).joinToString(",") { "0" })
        .replace("MAX_BUCKET_SIZE", bucketSize.toString())
        .replace("MAX_BUCKET_ELEMENT_SIZE", maxBucketElementSize.toString())
        .replace("REPLACE_RULE_COUNT", (rules.size + knowledgeBase.groupBy { it.head.name }.size + 2).toString())
        .replace("ARITMETHICS_COUNT", ARITHMETIC_OPERATIONS.filter { mapping.contains(it) }.size.toString())

    File("generated.circom").writeText(generatedCode)

    mapping.forEach { (name, index) ->
        println("'$name': $index,")
    }

    val treeJsonText = File("../tree.json").readText()
    var tree = ResolutionTree.parseJson(treeJsonText, mapping)
    val maxUniBody = rules.maxOf { it.value.maxOf { it.body.size } }
    //println(tree.getMaxDepth())
    tree = tree.standardize(unificationCount_input = branchingFactor, max_elements = maxPerdicateLength);
    File("input_tree.json").writeText(tree.toBFSJson(bucketSize = bucketSize))
}

private fun generateConstraints(
    rule_clauses: List<Clause>,
    stringBuilder: StringBuilder,
    additionalConstraintCount: Int = 0
) {

    stringBuilder.appendLine("\tsignal result[${rule_clauses.size+additionalConstraintCount}];")
    rule_clauses.forEachIndexed { ruleIndex, rule ->
        val constraintCount =  constraintCount(rule,additionalConstraintCount)
        val (headPositions, termPositions) = getArgumentPositions(rule)
        var constraintIndex = 0
        stringBuilder.appendLine("\tsignal intermediateResult${ruleIndex}[${constraintCount}];")
        if(rule.body[0].name == "="){
            stringBuilder.appendLine("\tintermediateResult${ruleIndex}[0] <== 1;")
        } else {
            // Generate the code for the matching terms in the head
            termPositions.map { (name, positions) ->

                if (headPositions.isNotEmpty() && headPositions.containsKey(name)) {
                    val headPosition = headPositions[name]!!.toList()
                    positions.forEach { position ->

                        headPosition.forEachIndexed { index, headPosition ->
                            // Old way: add("goal_args[${headPosition + 1}] == unified_body[${position.first}][${position.second + 1}]")
                            stringBuilder.appendLine("\tcomponent constraint${ruleIndex}_${constraintIndex} = IsEqual();")
                            stringBuilder.appendLine("\tconstraint${ruleIndex}_${constraintIndex}.in[0] <== unified_body[${position.first}][${position.second + 1}];")
                            stringBuilder.appendLine("\tconstraint${ruleIndex}_${constraintIndex}.in[1] <== goal_args[${headPosition + 1}];")
                            if (constraintIndex == 0) {
                                stringBuilder.appendLine("\tintermediateResult${ruleIndex}[${constraintIndex}] <== constraint${ruleIndex}_${constraintIndex}.out;")
                            } else {
                                stringBuilder.appendLine("\tintermediateResult${ruleIndex}[${constraintIndex}] <== constraint${ruleIndex}_${constraintIndex}.out + intermediateResult${ruleIndex}[${constraintIndex - 1}];")
                            }
                            constraintIndex++
                        }

                    }


                } else {
                    ""
                }
            }


            // Generate the code for the matching terms in the body
            val visited = mutableSetOf<Int>()


            termPositions.forEach { variable, positions ->
                // Match the body positions with each other
                positions.forEach { bodyPosition ->
                    positions.forEachIndexed { index, otherBodyPosition ->
                        if (bodyPosition != otherBodyPosition && !visited.contains(bodyPosition.first) && !visited.contains(
                                otherBodyPosition.first
                            )
                        ) {
                            // Old way: add("unified_body[${bodyPosition.first}][${bodyPosition.second + 1}] == unified_body[${otherBodyPosition.first}][${otherBodyPosition.second + 1}]")
                            visited.add(bodyPosition.first)
                            stringBuilder.appendLine("\tcomponent constraint${ruleIndex}_${constraintIndex} = IsEqual();")
                            stringBuilder.appendLine("\tconstraint${ruleIndex}_${constraintIndex}.in[0] <== unified_body[${bodyPosition.first}][${bodyPosition.second + 1}];")
                            stringBuilder.appendLine("\tconstraint${ruleIndex}_${constraintIndex}.in[1] <== unified_body[${otherBodyPosition.first}][${otherBodyPosition.second + 1}];")
                            if (constraintIndex == 0) {
                                stringBuilder.appendLine("\tintermediateResult${ruleIndex}[${constraintIndex}] <== constraint${ruleIndex}_${constraintIndex}.out;")
                            } else {
                                stringBuilder.appendLine("\tintermediateResult${ruleIndex}[${constraintIndex}] <== constraint${ruleIndex}_${constraintIndex}.out + intermediateResult${ruleIndex}[${constraintIndex - 1}];")
                            }
                            constraintIndex++
                        }
                    }
                }
            }
        }
        stringBuilder.appendLine("\tsignal resConstraint${ruleIndex};")
        stringBuilder.appendLine("\tresConstraint${ruleIndex} <== IsEqual()([intermediateResult${ruleIndex}[${constraintCount - 1}], ${constraintCount}]);")
        if(ruleIndex == 0){
            stringBuilder.appendLine("\tresult[${ruleIndex}] <== resConstraint${ruleIndex} * ruleSelector[$ruleIndex];")
        } else {
            stringBuilder.appendLine("\tresult[${ruleIndex}] <== resConstraint${ruleIndex} * ruleSelector[$ruleIndex] + result[${ruleIndex - 1}];")
        }
        stringBuilder.appendLine()


    }
}

fun constraintCount(rule: Clause,additionalConstraintCount: Int = 0): Int {
    val (headPositions, termPositions) = getArgumentPositions(rule)
    var constraintCount = 0
    if(rule.body[0].name == "="){
        return 1
    }
    // Generate the code for the matching terms in the head
    termPositions.map { (name, positions) ->

        if (headPositions.isNotEmpty() && headPositions.containsKey(name)) {
            val headPosition = headPositions[name]!!.toList()

            positions.forEach { position ->
                headPosition.forEachIndexed { index, headPosition ->

                    constraintCount++
                }
            }
        } else {
            ""
        }
    }
    val visited = mutableSetOf<Int>()
    termPositions.forEach { variable, positions ->
        // Match the body positions with each other
        positions.forEach { bodyPosition ->
            positions.forEachIndexed { index, otherBodyPosition ->

                if (bodyPosition != otherBodyPosition && !visited.contains(bodyPosition.first) && !visited.contains(
                        otherBodyPosition.first
                    )
                ) {
                    visited.add(bodyPosition.first)
                    constraintCount++
                }
            }
        }
    }

    return constraintCount
}

private fun getArgumentPositions(rule: Clause): Pair<MutableMap<String, MutableList<Int>>, MutableMap<String, MutableList<Pair<Int, Int>>>> {
    val headPositions = mutableMapOf<String, MutableList<Int>>()
    val termPositions = mutableMapOf<String, MutableList<Pair<Int, Int>>>()
    rule.head.terms.forEachIndexed { headIndex, term ->
        if (term is Variable) {
            if (!headPositions.containsKey(term.name)) {
                headPositions[term.name] = mutableListOf()
            }
            headPositions[term.name]!!.add(headIndex)
        }
    }
    rule.body.forEachIndexed { bodyIndex, predicate ->
        predicate.terms.forEachIndexed { termIndex, term ->
            if (term is Variable) {
                if (!termPositions.containsKey(term.name)) {
                    termPositions[term.name] = mutableListOf()
                }
                termPositions[term.name]!!.add(
                    Pair(
                        bodyIndex,
                        termIndex
                    )
                )
            }
        }
    }
    return Pair(headPositions, termPositions)
}

private fun generateRuleSelectors(
    rule_clauses: List<Clause>,
    mapping: Map<String, Int>,
    maxBodySize: Int,
    knowledgeUsageCounter: Int,
    stringBuilder: StringBuilder
) {
    var knowledgeUsageCounter1 = knowledgeUsageCounter
    rule_clauses.forEachIndexed { ind, rule ->
        rule.body.forEachIndexed { index, predicate ->
            val predicateEncodedName = if (
                Parser.SPECIAL_TERMS.any {
                    predicate.name.contains(it)
                }.not() &&
                Parser.OPERATORS.any {
                    predicate.name.contains(it)
                }.not()
            ) predicate.name else if (predicate.name == "=" && predicate.terms[1].name == "[]") mapping["true"] else mapping[predicate.name]
            if (index == 0) {
                stringBuilder.appendLine("\truleSelector_intermediate[${ind}][${index}] <== IsEqual()([unified_body[$index][0], ${predicateEncodedName}]);")
            } else {
                stringBuilder.appendLine("\tcomponent isEqual${ind}_${index} = IsEqual();")
                stringBuilder.appendLine("\tisEqual${ind}_${index}.in[0] <== unified_body[${index}][0];")
                stringBuilder.appendLine("\tisEqual${ind}_${index}.in[1] <== ${predicateEncodedName};")
                stringBuilder.appendLine("\truleSelector_intermediate[${ind}][${index}] <== isEqual${ind}_$index.out + ruleSelector_intermediate[${ind}][${index - 1}];")
            }
        }

        if (rule.body.size < rule_clauses.maxOf { it.body.size }) {
            var counter = rule.body.size
            while (counter < rule_clauses.maxOf { it.body.size }) {
                stringBuilder.appendLine("\tcomponent isZero${ind}_${counter} = IsZero();")
                stringBuilder.appendLine("\tisZero${ind}_${counter}.in <== unified_body[${counter}][0];")
                stringBuilder.appendLine("\truleSelector_intermediate[${ind}][${counter}] <== isZero${ind}_${counter}.out + ruleSelector_intermediate[${ind}][${counter - 1}];")
                counter++
            }
        }
        stringBuilder.appendLine("\truleSelector[${ind}] <== IsEqual()([ruleSelector_intermediate[${ind}][${maxBodySize - 1}], ${maxBodySize}]);")
    }
}

private fun initializeKnowledgeChecker(
    rule_clauses: List<Clause>,
    knowledgeBase: List<Clause>,
    stringBuilder: StringBuilder
): Int {
    val knowledgeBaseUsage = rule_clauses.filter { rule ->
        rule.body.size == knowledgeBase.groupBy { it.head.name }
            .count { rule.body.map { it.name }.contains(it.key) }
    }.sumOf { rule -> rule.body.size }
    if (knowledgeBaseUsage == 1) {
        stringBuilder.appendLine("\tcomponent knowledge = KnowledgeChecker();")
    } else if (knowledgeBaseUsage > 1) {
        stringBuilder.appendLine("\tcomponent knowledge[$knowledgeBaseUsage];")
        stringBuilder.appendLine("\tfor (var i = 0; i < $knowledgeBaseUsage; i++) {")
        stringBuilder.appendLine("\t\tknowledge[i] = KnowledgeChecker();")
        stringBuilder.appendLine("\t}")
    }
    return knowledgeBaseUsage
}

private fun addGoalHeader(
    name: String,
    maxUniBody: Int,
    maxPerdicateLength: Int,
    stringBuilder: StringBuilder
) {
    stringBuilder.appendLine("template Goal${name.replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() }}() {")
    stringBuilder.appendLine(
        "\tsignal input unified_body[${maxUniBody}][$maxPerdicateLength];\n" +
                "\tsignal input goal_args[$maxPerdicateLength];\n" +
                "\tsignal output c;\n" +
                "\tvar none = 0;"
    )
}

/**
 * List the Array type terms in the prolog program
 * @param rules The clauses of the prolog program in a map, where the key is the name of the predicate, and the value is a list of clauses
 * @return The names of the variables that are arrays
 */
fun findArrayPredicateVariables(rules: Map<String, List<Clause>>): Map<String, Int> {
    // List the arrays (predicates that's name is [] ) and print them
    // We need to find them recursively, because they can be nested in other predicates
    // If we find an array, save the name of its parent predicate
    val arrays = mutableMapOf<String, Int>()
    fun findArrays(predicate: Predicate, parent: Predicate? = null) {
        if (predicate.name == "[]") {
            arrays[parent!!.terms[0].name ?: "root"] = predicate.terms.size
        } else {
            predicate.terms.forEach { term ->
                if (term is Predicate) {
                    findArrays(term, predicate)
                }
            }
        }
    }
    rules.forEach { (name, rules) ->
        rules.forEach { rule ->
            rule.body.forEach { predicate ->
                findArrays(predicate)
            }
        }
    }

    return arrays.toMap()
}

/**
 * Find the maximum size of the arrays in the prolog program
 * @param rules The clauses of the prolog program in a map, where the key is the name of the predicate, and the value is a list of clauses
 * @return The maximum size of the arrays
 */
fun findArrayTermMaxSize(rules: Map<String, List<Clause>>): Int {
    // Find the maximum size of the arrays
    // We need to find them recursively, because they can be nested in other predicates
    // We first need to find the array predicate (predicate with name [])
    // Then we need to find the size of the array by recursively counting the number of elements in the array
    // Then we need to find the maximum size of the arrays


    val arraysSizes = mutableSetOf<Int>()

    fun findArraySize(predicate: Predicate): Int {
        if (predicate.terms.isEmpty()) {
            return 1
        } else if (predicate.name == "[]") {

            return predicate.terms.size + 1
        } else {
            var size = 0
            predicate.terms.forEach { term ->
                if (term is Predicate) {
                    size += findArraySize(term)
                }
            }
            return size
        }
    }

    rules.forEach { (name, rules) ->
        rules.forEach { rule ->
            rule.body.forEach { predicate ->
                arraysSizes.add(findArraySize(predicate))
            }
        }
    }

    return arraysSizes.maxOrNull()!!
}


fun clauseGenerateArithmeticsCheck(clause: Clause,index :Int): String {
    // Generate the code to check aritmetics in a clause
    val arithmeticPredicates = clause.body.filter { it.hasAritmetic() }
    val asserts = buildList {
        arithmeticPredicates.forEach { predicate ->
            add(predicateToString(predicate, clause.body.indexOf(predicate)))
        }
    }.map { "\t\truleSelector[$index]*($it) === 0;" }

    return buildString {
        asserts.forEach {
            appendLine(it)
        }
    }
}


fun predicateToString(predicate: Predicate, unificationIndex: Int, termIndexStart: Int = 0): String {
    // Helper function to handle the transformation
    fun termToString(term: Term): String = when {
        term.name.isInt() -> term.name
        term is Variable -> "unified_body[$unificationIndex][${predicate.terms.indexOf(term) + 1 + termIndexStart}]"
        term is Predicate -> predicateToString(
            term,
            unificationIndex,
            predicate.terms.filter { (it is Predicate).not() }.size + termIndexStart
        )

        else -> throw IllegalArgumentException("Unsupported term type")
    }

    return when (predicate.name) {
        "is" -> "${termToString(predicate.terms[0])} == ${termToString(predicate.terms[1])}"
        "/" -> {
            val numerator = termToString(predicate.terms[0])
            val denominator = termToString(predicate.terms[1])
            "($numerator - ($numerator % $denominator)) / $denominator"
        }

        "*" -> {
            // Check if any of the terms is a division
            val leftTerm = termToString(predicate.terms[0])
            val rightTerm = termToString(predicate.terms[1])
            if (predicate.terms[1] is Predicate && (predicate.terms[1] as Predicate).name == "/") {
                "$leftTerm * ($rightTerm)"
            } else {
                "$leftTerm * $rightTerm"
            }
        }

        else -> "${termToString(predicate.terms[0])} ${predicate.name} ${termToString(predicate.terms[1])}"
    }
}

fun String.isInt() = this.toIntOrNull() != null

fun arrayConstraintCount(rule_clauses: List<Clause>):Int {
    return rule_clauses.map { clause ->
        clause.head.terms.mapIndexed { index, term ->
            if (term is Predicate && term.name == "[]") {
                index
            } else {
                -1
            }
        }.filter { it != -1 }
    }.size
}