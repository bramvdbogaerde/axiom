## Style Guide

* Avoid many nested 'case of' expressions, if the matching is one Maybe's or Either's prefer monad (maybe with transformers such MaybeT or EitherT) instead.
* If the consequents of each branch in the 'case of' expression is short, use functions such as "maybe" or "either" to express the branching
* Avoid do notation for short statements (3-4 lines) prefer Applicative style where possible, if short enough, use ">>=" directly.
* Use eta-reduced expressions where possible, prefer point-free style.
