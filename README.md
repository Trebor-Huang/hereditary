# Setfuck

An esolang based on the hereditarily finite set universe.

Program examples can be seen at `example.sf`.

## Syntax

A program is a set of declarations. Each declaration is of the form
```
<Ident>(args) = <Term> ;
```
where `<Ident>` is a valid identifier, `<Term>` is a valid term, and `args` is a comma-separated list of `<Ident>`s.

To construct terms, the following operators are available:

- `<Ident>(args)` : applying defined declarations.
- `{ <Ident> : <Term> | <Term> }` : set comprehension, equivalent to Haskell `filter`.
- `{ <Term> | <Ident> : <Term> }` : set replacement, equivalent to Haskell `map`.
- `<Term> == <Term>` : set equality.
- `<Term> :: <Term>` : set membership.

The builtin constants are:
- `Empty()` : the empty set `{}`.
- `Pair(x, y)` : the set `{x, y}`.
- `Union(x)` : big union.
- `Power(x)` : powerset.

There are only sets in the language, booleans are represented by `false = {}` and `true = {{}}`.

## Build
Run `git clone` and `stack build` to build the executable.
`stack run -- file.sf` interprets a `.sf` file.

Issues and pull requests are welcome!
