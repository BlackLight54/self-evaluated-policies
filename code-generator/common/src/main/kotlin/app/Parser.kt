package hu.bme.app

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTreeWalker
import prologBaseListener
import prologLexer
import prologParser


class Parser : prologBaseListener() {
    companion object {
        fun parseProlog(code: String): MutableList<Clause> {
            return Parser().parse(code)
        }

        fun parsePredicate(predicateString: String): Predicate {
            val programString = "$predicateString."
            val clauses = parseProlog(programString)
            return clauses[0].head
        }

        fun parseTerm(termString: String): Term {
            val programString = "$termString."
            val clauses = parseProlog(programString)
            return clauses[0].head
        }

        fun parseClause(clauseString: String): Clause {
            val programString = "$clauseString."
            val clauses = parseProlog(programString)
            return clauses[0]
        }

        val OPERATORS = listOf(
            ":-",
            "-->",
            "?-",
            "dynamic",
            "multifile",
            "discontiguous",
            "public",
            ";",
            "->",
            ",",
            "\\+",
            "=",
            "\\=",
            "==",
            "\\==",
            "@<",
            "@=<",
            "@>",
            "@>=",
            "=..",
            "is",
            "=:=",
            "=\\=",
            "<",
            "=<",
            ">",
            ">=",
            ":",
            "+",
            "-",
            "/\\",
            "\\/",
            "*",
            "/",
            "//",
            "rem",
            "mod",
            "<<",
            ">>",
            "**",
            "^",
            "\\"
        )

        val BUILT_INS = listOf(
            "findall",
            "nl"
        )

        val SPECIAL_TERMS = listOf(
            "{}",
            "[]"
        ) // TODO: What are the special terms?
    }

    var zeroTermPedicates: Set<String> = setOf()

    val result = mutableListOf<Clause>()

    fun parse(code: String): MutableList<Clause> {
        val lexer = prologLexer(CharStreams.fromString(code))
        val tokens = CommonTokenStream(lexer)
        val parser = prologParser(tokens)

        ParseTreeWalker.DEFAULT.walk(this, parser.p_text())
        val res = result.toMutableList()
        result.clear()
        return res
    }

    //handles true/0, false/0 and other/0 type predicates
    fun atomToPredicate(atom: Atom): Predicate {
        zeroTermPedicates = zeroTermPedicates.plus(atom.name)
        return Predicate(atom.name, listOf())
    }

    override fun exitClause(ctx: prologParser.ClauseContext?) {

        if (ctx?.term()?.getChild(1)?.text == ":-") {
            val headTermNode = ctx.term().getChild(0)
            val bodyTermNode = ctx.term().getChild(2)
            var headPredicate = parseTerm(headTermNode!! as prologParser.TermContext)
            val bodyPredicates = parseListLinkedWithBinaryOperators(bodyTermNode!! as prologParser.TermContext)
//            println(Clause(headPredicate as Predicate, bodyPredicates.map { it as Predicate }))
            if (headPredicate is Atom) {
                // if head is an atom, then it must be a predicate with zero terms, e.g. true/0
                headPredicate = atomToPredicate(headPredicate)
            }
            result.add(Clause(headPredicate as Predicate, bodyPredicates.map {
                when (it) {
                    // if an element of body is atom, it must be a predicate with zero terms, e.g. true/0
                    is Atom -> atomToPredicate(it)
                    else -> it as Predicate
                }
            }))

        } else {
//            println(parseTerm(ctx?.term()!!))
            result.add(Clause(parseTerm(ctx!!.term()!!) as Predicate, listOf()))
        }
    }




    private fun parseListLinkedWithBinaryOperators(list: prologParser.TermContext): List<Term> {
        val result = mutableListOf<Term>()
//        println("term: ${list.text}")
//        list.children.map {
//            if (it is prologParser.TermContext) {
//                     println("checkedterm: ${it.text}")
//                 if (it.parent.getChild(1)?.text == ",") {
//                    result.add(parseTerm(it))
//                }
//            }
//        }
//        println("result: $result")
        if (list.childCount == 1) {
            result.add(parseTerm(list))
        } else if (list.getChild(1)?.text == ",") {
            result.add(parseTerm(list.getChild(0) as prologParser.TermContext))
            result.addAll(parseListLinkedWithBinaryOperators(list.getChild(2) as prologParser.TermContext))
        } else result.add(parseTerm(list))
//            throw Exception(list.text)
        return result
    }

    private fun parseUnaryOperator(ctx: prologParser.TermContext): Predicate {
        val terms = mutableListOf<Term>()
        val name = ctx.getChild(0).text!!
        terms.add(parseTerm(ctx.getChild(1) as prologParser.TermContext))
        return Predicate(name, terms)
    }

    private fun parseBinaryOperator(ctx: prologParser.TermContext): Predicate {
        val terms = mutableListOf<Term>()
        val name = ctx.getChild(1).getChild(0).text!!
        terms.add(parseTerm(ctx.getChild(0) as prologParser.TermContext))
        terms.add(parseTerm(ctx.getChild(2) as prologParser.TermContext))
        return Predicate(name, terms)
    }

    private fun parseCompoundTerm(ctx: prologParser.TermContext): Predicate {
        val terms = mutableListOf<Term>()
        val name = ctx.getChild(0).getChild(0).text!!
        val termList = ctx.getChild(2)
        terms.addAll(parseTermList(termList as prologParser.TermlistContext))
        return Predicate(name, terms)
    }

    private fun parseTermList(termListCtx: prologParser.TermlistContext): List<Term> {
        val childTerm = termListCtx.getChild(0)
        return parseListLinkedWithBinaryOperators(childTerm as prologParser.TermContext)
    }


    private fun parseCurlyBracedTerm(ctx: prologParser.TermContext): Term {
        if (ctx.text == "{}") return Atom("{}") //this line may be redundant
        val terms = mutableListOf<Term>()
        val name = "{}"
        val termList = ctx.getChild(2)
        terms.addAll(parseTermList(termList as prologParser.TermlistContext))
        return Predicate(name, terms)
    }

    private fun parseList(ctx: prologParser.TermContext): Term {
        if (ctx.text == "[]") return Atom("[]") //this line may be redundant
        val name = "[]"
        val terms = mutableListOf<Term>()
        val termList = ctx.getChild(1)
        terms.addAll(parseTermList(termList as prologParser.TermlistContext))
        if (ctx.getChild(3)?.text == "|") {
            terms.add(parseTerm(ctx.getChild(4) as prologParser.TermContext))
            return Predicate(
                name,
                terms + Predicate("|", listOf(parseTerm(ctx.getChild(2) as prologParser.TermContext)))
            )
        }
        return Predicate(name, terms)
    }

    private fun parseBuiltIn(ctx: prologParser.TermContext): Predicate {
        if (ctx.getChild(2) == null) return Predicate(ctx.getChild(0).text, listOf())
        return Predicate(
            ctx.getChild(0).text,
            parseListLinkedWithBinaryOperators(ctx.getChild(2) as prologParser.TermContext)
        )
    }

    private fun parseTerm(ctx: prologParser.TermContext): Term = when {
//        ctx.getChild(1)?.text == ";" -> parseDisjunction(ctx)
        // parse true, because true/0 is a built-in predicate
        ctx.childCount == 1 && ctx.getChild(0).text == "true" -> Predicate("true", listOf())
        // parse built-in predicates
        ctx.getChild(0).text in BUILT_INS -> parseBuiltIn(ctx)
        // parse compound terms(i.e. atom(termlist))
        ctx.childCount == 4 && ctx.getChild(1).text == "(" && ctx.getChild(3).text == ")" -> parseCompoundTerm(ctx)
        // parse binary operators
        ctx.childCount == 3 && ctx.getChild(1).text in OPERATORS -> parseBinaryOperator(ctx)
        // parse unary operators
        ctx.childCount == 2 && ctx.getChild(0).text in OPERATORS -> parseUnaryOperator(ctx)
        // parse list terms
        ctx.getChild(0).text == "[" && ctx.getChild(ctx.childCount - 1).text == "]" -> parseList(ctx)
        // parse curled terms
        ctx.getChild(0).text == "{" && ctx.getChild(ctx.childCount - 1).text == "}" -> parseCurlyBracedTerm(ctx)
        // parse braced terms
        ctx.getChild(0).text == "(" && ctx.getChild(2).text == ")" -> parseTerm(ctx.getChild(1) as prologParser.TermContext)
        // parse variables
        ctx.text.first().isUpperCase() || ctx.text.first() == '_' -> Variable(ctx.text)
        ctx.text in zeroTermPedicates -> atomToPredicate(Atom(ctx.text))
        else -> Atom(ctx.text)
    }.also {
//        if (it.name == ",") throw Exception("Comma in term: ${ctx.text}")
//        if (it.name  == ";")  throw Exception("Semicolon in term: ${ctx.text}")
    }

//    private fun parseDisjunction(ctx: prologParser.TermContext): Term {
//        ctx.ge
//    }


}