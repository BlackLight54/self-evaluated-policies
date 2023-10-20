package hu.bme.app

import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTree
import org.antlr.v4.runtime.tree.ParseTreeWalker
import prologBaseListener
import prologLexer
import prologParser


class Parser(val code: String) : prologBaseListener() {

    val result = mutableListOf<Clause>()

    fun parse() {
        val lexer = prologLexer(CharStreams.fromString(code))
        val tokens = CommonTokenStream(lexer)
        val parser = prologParser(tokens)
        val tree: ParseTree = parser.p_text()

        val walker = ParseTreeWalker()
        walker.walk(this, tree)
    }


    override fun exitClause(ctx: prologParser.ClauseContext?) {

        println("Clause:")
        if (ctx?.term()?.getChild(1)?.text == ":-") {
            val headTermNode = ctx.term().getChild(0)
            val bodyTermNode = ctx.term().getChild(2)
            println("Head: ${headTermNode?.text}")
            println("Body: ${bodyTermNode?.text}")
            val headPredicate = parsePredicate(headTermNode!! as prologParser.TermContext)
            val bodyPredicates = parseBody(bodyTermNode!! as prologParser.TermContext)
            println(Clause(headPredicate, bodyPredicates))

        } else {
            println("Fact: ${ctx?.term()?.getChild(0)?.text}")
            println(parsePredicate(ctx?.term()!!))
        }


    }

    fun parseBody(body: prologParser.TermContext): List<Predicate> {
        val result = mutableListOf<Predicate>()
        if (body.getChild(1).text == "(" && body.getChild(body.childCount - 1).text == ")") {
            result.add(parsePredicate(body))
        } else {
            for (i in 0 until body.childCount) {
                if (body.getChild(i).text == ",") continue
                result.add(parsePredicate(body.getChild(i) as prologParser.TermContext))
            }
        }
        return result
    }

    fun parsePredicate(ctx: prologParser.TermContext): Predicate {
        var name = ""
        val terms = mutableListOf<Term>()
        // parse atom(termlist) format predicates
        if (ctx.childCount == 4 && ctx.getChild(1).text == "(" && ctx.getChild(3).text == ")") {
            name = ctx.getChild(0).text!!
            val termList = ctx.getChild(2)
            terms.addAll(parseTermList(termList as prologParser.TermlistContext))
        }
        // parse binary operators
        else if (ctx.childCount == 3 && ctx.getChild(1).text != ",") {
            name = ctx.getChild(1).getChild(0).text!!
            terms.add(parseTerm(ctx.getChild(0) as prologParser.TermContext))
            terms.add(parseTerm(ctx.getChild(2) as prologParser.TermContext))
        }
        // parse unary operators
        else if (ctx.childCount == 2) {
            name = ctx.getChild(0).text!!
            terms.add(parseTerm(ctx.getChild(1) as prologParser.TermContext))
        }
        // parse compund terms
        else if (ctx.childCount > 2) {
            name = ctx.getChild(0).text!!
            terms.addAll(parseCompundTerm(ctx))
        }

        return Predicate(name, terms)
    }

    fun parseTermList(termListCtx: prologParser.TermlistContext): List<Term> {
        val result = mutableListOf<Term>()
        val child = termListCtx.getChild(0)
        if (child.childCount == 1) {
            result.add(parseTerm(child as prologParser.TermContext))
        } else {
           result.addAll(parseCompundTerm(child as prologParser.TermContext))
        }
        println("TermList:$result")
        return result
    }

    fun parseCompundTerm(compoundTerm: prologParser.TermContext): List<Term> {
        val result = mutableListOf<Term>()
        for (j in 0 until compoundTerm.childCount) {
            if (compoundTerm.getChild(j).text == ",") continue
            result.add(parseTerm(compoundTerm.getChild(j) as prologParser.TermContext))
        }
        return result
    }


    fun parseTerm(ctx: prologParser.TermContext) = when {
        ctx.text.first().isUpperCase() -> Variable(ctx.text)
        ctx.text.contains('(') -> parsePredicate(ctx)
        else -> Atom(ctx.text)
    }
}