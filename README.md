# Axiom

A domain-specific language for specifying and executing operational semantics and program analysis rules.

## Overview

Axiom is a declarative language for defining operational semantics, abstract machines, and program analysis specifications. It allows you to write high-level semantic rules that are automatically compiled to executable Haskell code with a built-in backward-chaining solver.

**Key Features:**
- **Declarative syntax** for defining transition rules and rewrite functions
- **Automatic solver** implementing backward search with goal shelving and caching to handle infinite recursion
- **Embedded Haskell** expressions using `${...}` syntax for complex operations
- **Rich type system** with support for sets, maps, lattices, and user-defined functors
- **Language Server Protocol (LSP)** support for editor integration with diagnostics and symbol navigation

## Installation

### Prerequisites

- **GHC** (Glasgow Haskell Compiler) - tested with GHC 9.6+

### Building from Source

```bash
git clone <repository-url>
cd axiom
cabal build
```

This will build both the `axiom-analysis` library and the `axiom` executable.

## Usage

### Basic Commands

Axiom provides several commands for working with semantics specifications:

**Type check a program:**
```bash
cabal run axiom -- check FILE [-v]
```

**Run the solver on test queries:**
```bash
cabal run axiom -- runsolver FILE
```

**Generate and execute Haskell code:**
```bash
cabal run axiom -- runcodegen FILE
```

**Generate Haskell code only:**
```bash
cabal run axiom -- codegen FILE
```

**Start interactive solver debugger:**
```bash
cabal run axiom -- debug FILE
```

**Generate LaTeX documentation:**
```bash
cabal run axiom -- latex FILE
```

**Start LSP server:**
```bash
cabal run axiom -- lsp
```

### Writing Semantics Specifications

A basic Axiom program consists of:

1. **Syntax declarations** defining your abstract syntax and state space
2. **Rewrite functions** for auxiliary computations
3. **Transition declarations** specifying judgment forms
4. **Rules** defining the operational semantics

**Example** (`simple.sem`):

```
syntax {
    e in Exp ::= lambda(x, e) | app(e, e) | x;
    v in Value ::= closure(x, e, env);
    x in Var;
    env in Map(Var, Value);
    input in Input ::= inp(e, env);
};

transition Input ~> Value;

rules {
    rule "Eval-Lambda"
    [ ]
    =>
    [ inp(lambda(x, e), env) ~> closure(x, e, env) ];

    rule "Eval-Var"
    [ env(x) = v ]
    =>
    [ inp(x, env) ~> v ];
};
```

Check the program:
```bash
cabal run axiom -- check simple.sem
```

See `examples/lambda.sem` for a more complete example with stores, closures, and control flow.

### Embedding Haskell Code

You can embed arbitrary Haskell expressions using `${...}`:

```
rule "Add"
[ ]
=>
[ result = ${ x + y } ];
```

For top-level imports use `{{{ ... }}}`:

```
{{{
import Data.Set (Set)
import qualified Data.Set as Set
}}}
```

For other declarations use `[[[ ... ]]]` instead.

## Language Server

Axiom includes an LSP server that provides real-time syntax and type error
diagnostics, as well as symbol navigation and workspace symbols.

### Helix Configuration

Add the following to your Helix `languages.toml`:

```toml
[[language]]
name = "axiom"
scope = "source.axiom"
file-types = ["sem"]
comment-token = "%"
language-servers = ["axiom-lsp"]

[language-server.axiom-lsp]
command = "axiom"
args = ["lsp"]
```

After configuring, Helix will automatically start the language server when you open `.sem` files.

### VS Code / Other Editors

For VS Code or other LSP-compatible editors, configure them to run:

```bash
axiom lsp
```

as the language server command for files with the `.sem` extension.

## License

See LICENSE file for details.
