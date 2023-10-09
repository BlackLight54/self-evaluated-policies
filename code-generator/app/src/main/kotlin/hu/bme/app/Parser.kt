package hu.bme.app

object Parser {
    val SPECIAL_TERMS = listOf(" is ", ">", "<", ">=", "=<",  "\\=", "==","+","-","*","/","\\")
    fun parseProlog(prologCode: String): List<Clause> {
        val clauses = mutableListOf<Clause>()

        val mergedLines = preprocessPrologCode(prologCode)
        for (line in mergedLines) {
            clauses.add(parseClause(line))
        }

        return clauses
    }


    fun preprocessPrologCode(prologCode: String): List<String> {
        val lines = prologCode.lines()
        val mergedLines = mutableListOf<String>()
        var currentLine = StringBuilder()

        for (line in lines) {
            val trimmedLine = line.trim()
            if (trimmedLine.isNotEmpty()) {
                currentLine.append(trimmedLine).append(" ")
                if (trimmedLine.endsWith('.')) {
                    mergedLines.add(currentLine.toString())
                    currentLine = StringBuilder()
                }
            }
        }

        if (currentLine.isNotEmpty()) {
            mergedLines.add(currentLine.toString())
        }

        return mergedLines
    }


    fun parseClause(clauseStr: String): Clause {
        val parts = clauseStr.split(":-")
        val head = parsePredicate(parts[0].trim().trimEnd('.'))

        val body = if (parts.size > 1) {
            splitPredicates(parts[1].trim()).map { parsePredicate(it.trim()) }
        } else {
            emptyList()
        }

        return Clause(head, body)
    }

    fun splitPredicates(bodyStr: String): List<String> {
        val predicates = mutableListOf<String>()
        var start = 0
        var depth = 0
        var listDepth = 0  // to handle lists

        for ((index, char) in bodyStr.withIndex()) {
            if (char == '(') depth++
            if (char == ')') depth--
            if (char == '[') listDepth++
            if (char == ']') listDepth--
            if (char == ',' && depth == 0 && listDepth == 0) {
                predicates.add(bodyStr.substring(start, index).trim())
                start = index + 1
            }
        }

        predicates.add(bodyStr.substring(start).trim())

        return predicates
    }

    fun parsePredicate(predStr: String): Predicate {
        val name = predStr.substringBefore("(").trim()
        // Handle arithmetics
        if (SPECIAL_TERMS.any { predStr.contains(it) }) {
            val predicate = parseSpecialPredicate(name)
            return if(predicate is Predicate) predicate as Predicate else error("$predStr is not a predicate")
        }
        val termsStr = if (predStr.contains("(")) predStr.substringAfter("(").trimEnd(')', '.',',').trim() else ""
        val terms = if (termsStr.isNotEmpty()) {
            termsStr.split(",").map { parseTerm(it.trim()) }
        } else {
            emptyList()
        }

        return Predicate(name, terms)
    }

    // Parse arithmetics
    fun parseSpecialPredicate(predStr: String): Term {
        // Return if empty recursive call
        if (predStr.isEmpty()) return Predicate("", emptyList())
        // Find the special term name in the specialTerms list
        val specialTerm = SPECIAL_TERMS.find { predStr.contains(it) } ?: predStr
        return if(predStr != specialTerm) {
            // Split the predicate string into the left and right side of the special term
            val parts = predStr.split(specialTerm)
            val left = parseTerm(parts[0].trim())
            val rightPredicate = parseSpecialPredicate(parts[1].trim())
            // Return the predicate
            Predicate(specialTerm, listOf(left, rightPredicate))
        } else {
            parseTerm(predStr)
        }

    }

    fun parseTerm(termStr: String): Term {
        val trimmedTerm = termStr.trimEnd('.').trim()

        // Handle list structures, assuming simple cases for now
        if (trimmedTerm.startsWith("[") && trimmedTerm.endsWith("]")) {
            return Atom(trimmedTerm)
        }

        return when {
            trimmedTerm.first().isUpperCase() -> Variable(trimmedTerm)
            else -> Atom(trimmedTerm)
        }
    }


}