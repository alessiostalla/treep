### Design Decisions

- *No symbols* In Lisp, symbols fulfill mainly two needs: providing  _identity_ and a _mapping_ between names and identities. Historically, symbols were in the language before strings were introduced, so they sometimes doubled as a crude means to represent character data, but that's long become obsolete. In Treep, we have forms with _identity_, and some forms act as _namespaces_ for _mapping_ names to identities. So, the need for built-in symbols is not as strong as in Lisp.
- *Paths* Also, historically in Lisp there was a single global namespace for symbols, and when that became potentially too overcrowded, packages were introduced, providing one additional layer of disambiguation. Still, package names live in a single global namespace in most Lisps. In Treep, we support multi-layered paths that can represent complex structures such as project:module:package:class:member.

## License

To the general public, Treep is distributed under the AGPL license, which is quite strict.
However, individuals and organizations that want to use Treep for their purposes may obtain a friendlier license (either for free or for a fee, depending on the intended use). Please write to the author.
