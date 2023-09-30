package hu.bme.app

sealed class Term

data class Atom(val name: String) : Term()
data class Variable(val name: String) : Term() {
    override fun toString(): String {
        return "Variable(name='$name')"
    }
}

data class Predicate(val name: String, val terms: List<Term>)

data class Clause(val head: Predicate, val body: List<Predicate>)
