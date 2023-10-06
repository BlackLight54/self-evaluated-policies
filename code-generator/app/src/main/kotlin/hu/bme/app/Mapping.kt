package hu.bme.app

fun createMapping(clauses: List<Clause>) :Map<String,Int>{
    val knowledgeBase = clauses.filter { it.body.isEmpty() }
    var counter = 1;
    val mapping = mutableMapOf<String, Int>()
    knowledgeBase.forEach { clause ->
        clause.head.terms.forEach { term ->
            if (!mapping.contains(term.name)) {
                mapping[term.name] = counter++
            }
        }
    }
    val rules = clauses.filter { it.body.isNotEmpty() }
    rules.forEach { clause ->
        clause.head.terms.forEach { term ->
            if (!mapping.contains(term.name) && term is Atom) {
                mapping[term.name] = counter++
            }
        }
        clause.body.forEach { predicate ->
            predicate.terms.forEach { term ->
                if (!mapping.contains(term.name) && term is Atom) {
                    mapping[term.name] = counter++
                }
            }
        }
    }
    // Add mapping for predicates
    knowledgeBase.forEach { clause ->
        if (!mapping.contains(clause.head.name)) {
            mapping[clause.head.name] = counter++
        }
    }
    rules.forEach { clause ->
        if (!mapping.contains(clause.head.name)) {
            mapping[clause.head.name] = counter++
        }
        clause.body.forEach { predicate ->
            if (!mapping.contains(predicate.name)) {
                mapping[predicate.name] = counter++
            }
        }
    }
    mapping["true"] = counter++
    mapping["false"] = counter++
    // Add mapping for variables
    rules.forEach { clause ->
        clause.head.terms.forEach { term ->
            if (!mapping.contains(term.name) && term is Variable) {
                mapping[term.name] = counter++
            }
        }
        clause.body.forEach { predicate ->
            predicate.terms.forEach { term ->
                if (!mapping.contains(term.name) && term is Variable) {
                    mapping[term.name] = counter++
                }
            }
        }
    }
    return mapping
}