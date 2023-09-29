package hu.bme.app

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class ParserTest {

    @Test fun testParseAtom() {
        val atom = Parser.parseTerm("a")
        assertEquals(Atom("a"), atom)
    }

    @Test fun testParseVariable() {
        val variable = Parser.parseTerm("X")
        assertEquals(Variable("X"), variable)
    }

    @Test fun testParsePredicate() {
        val predicate = Parser.parsePredicate("p(a, X)")
        assertEquals(Predicate("p", listOf(Atom("a"), Variable("X"))), predicate)
    }

    @Test fun testParseClause() {
        val clause = Parser.parseClause("p(a, X) :- q(X, Y), r(Y)")
        assertEquals(Clause(
            Predicate("p", listOf(Atom("a"), Variable("X"))),
            listOf(
                Predicate("q", listOf(Variable("X"), Variable("Y"))),
                Predicate("r", listOf(Variable("Y")))
            )
        ), clause)
    }

    @Test fun testParseProlog() {
        val prologCode = """
            p(a, X) :- q(X, Y), r(Y).
            q(X, Y) :- s(X, Y).
            r(Y) :- t(Y).
            s(a, b).
            t(b).
        """.trimIndent()

        val clauses = Parser.parseProlog(prologCode)
        assertEquals(listOf(
            Clause(
                Predicate("p", listOf(Atom("a"), Variable("X"))),
                listOf(
                    Predicate("q", listOf(Variable("X"), Variable("Y"))),
                    Predicate("r", listOf(Variable("Y")))
                )
            ),
            Clause(
                Predicate("q", listOf(Variable("X"), Variable("Y"))),
                listOf(
                    Predicate("s", listOf(Variable("X"), Variable("Y")))
                )
            ),
            Clause(
                Predicate("r", listOf(Variable("Y"))),
                listOf(
                    Predicate("t", listOf(Variable("Y")))
                )
            ),
            Clause(
                Predicate("s", listOf(Atom("a"), Atom("b"))),
                emptyList()
            ),
            Clause(
                Predicate("t", listOf(Atom("b"))),
                emptyList()
            )
        ), clauses)
    }
}