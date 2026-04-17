# Treep

If Lisp is a LISt Processor, Treep is a (syntax) TREE Processor. A dedicated high-level language to design languages (also known as DSLs), written on top of the Common Lisp Object System, CLOS.

## Design Decisions

- *Objects are code* while Treep, like Lisp, follows a "code-is-data" approach, the constituents of the code are different: Lisp is defined in terms of CONSes and symbols (plus other necessary objects such as numbers and strings), while Treep is defined in terms of CLOS objects. Homoiconicity is given by the fact that Treep has primitives to define, combine and manipulate those objects, and to interpret them as code. Additionally, Treep gives its CLOS objects a well-defined syntax so they can be printed to text and read back.
- *No symbols* In Lisp, symbols fulfill mainly two needs: providing  _identity_ and a _mapping_ between names and identities. Historically, symbols were in the language before strings were introduced, so they sometimes doubled as a crude means to represent character data, but that's long become obsolete. In Treep, we have forms with _identity_, and some forms act as _namespaces_ for _mapping_ names to identities. So, the need for built-in symbols is not as strong as in Lisp.
- *Paths* Also, historically in Lisp there was a single global namespace for symbols, and when that became potentially too overcrowded, packages were introduced, providing one additional layer of disambiguation. Still, package names live in a single global namespace in most Lisps. In Treep, we support multi-layered paths that can represent complex structures such as project:module:package:class:member.

### Symbols and Compiling to Lisp

While Treep itself has no symbols, being defined in terms of CLOS objects and metaobjects implies the use of symbols at some point. Treep solves this by using uninterned symbols until the code is compiled to Lisp. When that happens, a target package is designated (by the user, or automatically by the system): Treep will import all the relevant uninterned symbols into that package, thus making the objects and classes conveniently accessible from Lisp.

## License

To the general public, Treep is distributed under the AGPL license, which is quite strict.
However, individuals and organizations that want to use Treep for their purposes may obtain a friendlier license (either for free or for a fee, depending on the intended use). Please write to the author.
