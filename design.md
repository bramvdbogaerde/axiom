# Solving Infinite backward search

## Sources of non-finiteness

In some cases, the solver can run infinitly without producing a result. For instance,

```analysislang
rule "Rule-Rec"
[ f(X, Y) ] =>
[ f(z(X), Y) ]
```

To arrive at the conclusion in the consequent (i.e., "f(z(X), Y)", the solver needs to ensure that "f(X, Y)" holds. Typically in a backwards solver (such as the one Prolog) this is accomplished by recursively calling the solver for the terms with the antecedent. This, however, can lead to an infinite recursion.

An in-out cache elliminates this problem by tracking two things: (1) whetever a predicate is in the process of being solved (2) which facts have been derived earlier. The first part is called the "in" and the second part is called the "out". 

## Shelving goals

The idea of applying this caching mechanism in the solver is to "shelve" goals whenver we notice that there is a unifiable term in the in-cache. One should be careful with this though, as the unifiable term can be come un-unfiable in the future (for example due to other unification actions). 

To keep track of shelved goals, the "SearchState" should be expanded to contain the following fields:

* _searchInCache: a set of terms that are still being solved

Next, to track which facts have been established, we need to update the "SearchCtx" structure to include the following additional fields:

* _searchCache: a mapping from functor names to facts that can be unified with without looking at the rules themselves.

A typical run of the solver then proceeds as follows:

1. The solver tries to find a solution to its goals as usual.
2. Whenever the solver recursively tries to solve a goal, that goal is added to the _searchInCache, alongside with a continuation which adds the consequent to the _seachCache
3. Whenver the solver tries to find a solution for a subgoal that is already in the in-cache, it simply does not try to solve that subgoal and gives up on original goal, UNLESS the goal can be solved by looking at the _searchCache. 
4. Repeat this process until the cache no longer changes its contents.

# Embedding Haskell

Arbitrary Haskell code can be embedded in the derivation rules and rewrite rules of the language. This is to enable an "escape hatch" for more complex operations that are difficult to expression in the language itself, but should be used with caution. Each used Haskell escape hatch must be a pure function, so the things that can be accomplished is more limited to, say, PLT-Redex.

## Syntax

Haskell code can be embedded in the context of rewrite functions and rules by using the `${...}` syntax. This will splice a Haskell function into the AST of a rule or rewrite function.

## Typing

Embedded Haskell code is typed according to the types of the surrounded context. Before the code generation phase, we do not do any type checking of the Haskell code itself, since we do not now the types of any user-defined function or preludes.
The surrounded context always determines the type of a Haskell expression, for instance, the code depicted below types 42 as "Integer". This is not because "42" *is* an integer, but rather because `v` is declared as "Integer" and the '=` requires that the left and right hand-side have the same type.

```analysislang
syntax {
	v in Integer;
}

rules {
	rule "Test" [ ] [ v = ${42} ];
};
```
AnalysisLang contains several builtin functions that can be used in conjunction with Haskell expressions:

* Integer
* String
* Set Value (where Value is one of the types listed here)

In the solver, the return type of an Haskell expression is wrapped into boxes for one of these types wherever possible. In the example above, the programmer does not need to wrap manually since "Integer" is a supported Haskell type. Howeve,r when the expected type would be an arbitrary functor, the Haskell expression should include code to return a functor of the appropriate type by using functions from the "Language.AST" module.

## Arguments

Haskell expression can refer to variables used elsewhere in the rule. In order to do this, however, each variable must be ground at the time the Haskell expression is executed. The solver will automatically attempt to re-order its goals to ensure that this the case and otherwise return an error if it was unable to do so. 

### Reordering of goals

To ensure that all variables are ground before the Haskell expression is executed. The solver constructs a local dependency graph between variables and then performs a topological sorting to determine which goals should be solved first.

### Haskell Expression Representation

During the code generation phase, the AST is lifted into a template-haskell expression. While doing so, 'HaskellExpr' nodes are parsed to valid template-haskell expressions and inserted as the following data type in the AST:

```
data HaskellHatch = HaskellHatch {
		freeVars :: [String],
		execute  :: Map String PureTerm -> PureTerm
	}
```

The execute function is a function generated by the CodeGen module, and contains local variable bindings for each free variable that refers to variables in the rule or rewrite function. The generated function also contains, depending on the expected type of the Haskell expression, a wrapper converting the type of the Haskell expression to a term.

This execute function is constructed as follows. Say that we have the following AnalysisLang rule:

```
rule "Test" [ a0 = 42 ] [ a1 = ${ a0 + 1 } ]
```

The rule depicted above, references the logical variable "a" in its embedded Haskell expression. Since the logical variable is unified with the value of 42 which has type "IntType", the expression $a + 1$ compiles correctly. Moreover, since variables "a" have type "Int" the result of the embedded Haskell expression should also be an integer. This brings us to the final version of the embedded Haskell expression which will be included as a function in the the abstract syntax tree after code generation through the `HaskellHatch` datatype (cf. above).

```
\mapping ->
	let a0 = fromJust $ asType IntType $ fromJust $ Map.lookup "a0" mapping
	in TermValue $ (IntLit $ a0 + 1) (Just IntType) dummyRange
```

## Type Declarations

Since Haskell expressions can return arbitrary values that are not supported in term language as literals, the user can define their own types in syntax declarations blocks that are translated into a `Dynamic`-like datatype when code generation is applied.


For instance, to define an "environment" that consists of mappings from variables to addresses, one could write:

```analysislang
env in ${Map String Adr}
```

The types mentioned in the embedded Haskell type could refer both to built-in or imported types or to types defined in syntax rules. In case of the latter, each type should be prefixed by a `#'. For insance to refer to term type `Adr`, one should instead write:

```
env in ${Map String #Adr}
```

## Embedding arbitrary Haskell code as a prelude

The langauge supports embedding arbitrary Haskell code into the generated program as a prelude by uisng the "{{{ ... }}}" syntax on the top-level of the program.

For instance, the program generated could be instructed to include an import declaration for `Data.Maybe` by including the following declaration in the AnalysisLang file.

```analysislang
{{{
import Data.Maybe
}}}
```

These declarations can occur at any location in the program, but are always added to the beginning of the generated program in the order in which they occur in the AnalysisProgram.

## Future Extensions

Side-effects are often obtained through monads in Haskell. It would be interesting to embed these side-effects into the solver monad so that the user can write rules that perform these side-effects, for instance for rule profiling through a state monad.

Configuring which monad should be used can be accomplished in a similar way as Happy or Alex (i.e., by specifying the type of the monad alongside with a `run` function)

# Supports for Sets and Lattices

AnalysisLang supports a set of primitives for working with sets and lattices. Most of this support is accomplished through embedded Haskell expressions which can refer to arbitrary types (cf. above).
