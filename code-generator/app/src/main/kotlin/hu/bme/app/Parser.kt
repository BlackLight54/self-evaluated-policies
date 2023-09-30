package hu.bme.app

object Parser {
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
        val termsStr = if (predStr.contains("(")) predStr.substringAfter("(").trimEnd(')', '.',',').trim() else ""
        val terms = if (termsStr.isNotEmpty()) {
            termsStr.split(",").map { parseTerm(it.trim()) }
        } else {
            emptyList()
        }

        return Predicate(name, terms)
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