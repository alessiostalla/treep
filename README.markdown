# Treep

Treep is an (Abstract Syntax) Tree-Processing language. It aims to be the core of a "language workbench" type of application, to aid in the implementation of: 
 * Domain-Specific Languages (DSLs)
 * Static Analysis Tools
 * Editors (projectional and text-based)
 * Translators/Generators
 * Everything else revolving around the implementation of Programming Languages (i.e. formal languages).

As such, Treep is a specialized language: you're not expected to write an entire application in it. Rather, it's a convienent toolbox for building other languages (interpreters, compilers, editors, ...).

In Treep, we start by defining the abstractions that make up our language (known as the concepts, metamodel, AST, etc. in other tools). Then, we can:
 * Define *transformations* from our abstractions into other notations (e.g. an interpreter, a compiler, a documentation generator, a diagram generator, etc.)
 * Attach extra information to our abstractions (e.g. documentation, syntax rules, editor definitions, etc.)
 * Export the abstractions to other tools using APIs

## Goals

 * Provide a set of object-oriented building blocks for designing and implementing formal languages
 * Provide an abstract language to express those building blocks
 * Make it easy to represent the language as text (in S-expression notation, XML, etc.)
 * Provide a framework for tree-to-tree transformations (macros)
 * Make it self-documenting (documentation as metadata attached to the code)
 * Provide a useful runtime environment for the above, including:
   * A self-hosting (metacircular) interpreter
   * A compiler
   * A REPL
   * A debugger
 * Provide useful higher-level building blocks such as:
   * A type system with inference and some support from the compiler
   * A web-based code (model) browser
   * A language for syntax (for parsing and unparsing, i.e., for defining a CST or Concrete Syntax Tree)
   * Graphical transformations, visualizations, and such
   * Integrations with existing tools (e.g. MPS, web-based projectional editors, etc.)
 * Write thorough documentation for all the above
 * Generate interest in the project, spread the word
   
## Non-goals

 * To write a full IDE or projectional editor
 * To write an industrial-grade optimizing compiler
 
## Design Principles

 * Code is made of *forms* (expressions) which are CLOS objects (i.e., instances of classes).
 * Forms may *contain* other forms (e.g. a function declaration will contain parameter declarations and a body),
   thus they're organized in a tree.
 * Names are symbols, not strings.
 * Some forms may use symbols or other means to reference other forms/definitions, so the tree is actually a graph with some preferential edges (the parent-child relationships).
 * Symbols can act as containers of other symbols, thus they are hierarchical (foo:bar:baz) and they can easily map to languages with multiple levels of namespacing (e.g., package.class.member).
 * Environments (also known as symbol tables) are first-class objects.
 * Loading code (models) has no side effects. The load function returns a module which consists of:
   * A symbol with all the newly read symbols interned;
   * An environment with the new definitions added.
 * The system keeps track of who-calls-who so that, when installing new definitions into an environment, dependant
   functions can be recompiled (e.g., if the signature of a function changed).
 * Everything can be inspected. Functions retain information about their arguments (including types).

## Usage

At the present time, the language is not yet usable in practice, too many pieces are missing, in particular, a REPL. Stay tuned.

## Implementation

Treep is implemented in Common Lisp and CLOS. While we believe this is an advantage, and Lisp enthusiasts will agree, the idea is that it won't be necessary to read or write any Lisp to use Treep.

We're not there yet, but that's a goal.

## Installation

Treep is developed and tested on ABCL and SBCL.
 * Install a Lisp implementation, preferably ABCL or SBCL. ABCL runs on the JVM while SBCL is a native executable available on all major platforms.
 * Install Quicklisp.
 * Load treep with ASDF.

## Implementation and Building

Treep is structured as a tower of languages.
 * At level -1, the foundation, we have Common Lisp.
 * At level 0, the ground floor, we have the minimal set of classes and functions to define new Treep abstractions and transformations, and the core set of abstractions, written in Common Lisp.
 * At level 1, we rewrite the abstractions and transformations in Treep. Level 1 Treep is a specialized language for defining abstractions and transformations into core abstractions.
 * At level 2, we define an editor abstraction, type system, documentation system and other high-level systems. Level 2 Treep is a coherent suite of dedicated languages for all language-engineering tasks.

Each new level may require additional support from the levels below, so as the implementation progresses the above could change.

## License

To the general public, Treep is distributed under the AGPL license, which is quite strict.
However, individuals and corporations that want to use Treep for their purposes can obtain a friendlier license (either for free or for a fee, depending on the intended use). Please write to the author.