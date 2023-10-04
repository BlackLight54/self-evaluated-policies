package hu.bme.app

sealed class Term(val name: String) {
    open fun encode(mapping: Map<String, Int>): List<Int> {
        return listOf(mapping[name] ?: error("Unknown term: $name"))
    }
}

class Atom(name: String) : Term(name)
class Variable(name: String) : Term(name) {
    override fun toString(): String {
        return "Variable(name='$name')"
    }
}

class Predicate(name: String, val terms: List<Term>) : Term(name){
    override fun encode(mapping: Map<String, Int>): List<Int> {
        val result = mutableListOf<Int>()
        result.add(mapping[name] ?: error("Unknown predicate: $name"))
        terms.forEach { term ->
            result.addAll(term.encode(mapping))
        }
        return result
    }
}

data class Clause(val head: Predicate, val body: List<Predicate>)
