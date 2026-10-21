# Parsing

Main-module functions cannot be named `dump`, `sizeof`, `typeof`, or `isreftype`.
These names have builtin syntax, so the compiler reports a redefinition error at the function
declaration. Methods, C declarations, and qualified functions in other modules can use these names.

Parenthesized expressions may span lines, including a line break before the closing `)`.
Line comments in a multiline condition preserve its grouping and any following `&&` or `||`.

A struct initializer may put its opening `{` on the next line after the type name,
including qualified names and generic types. An explicit `;` still ends the preceding statement.

`Parser.preload_source(path, source)` supplies a source snapshot for subsequent parsing and
sibling declaration scans. `clear_preloaded_sources()` restores disk reads for those paths.
Parallel prepasses and full parses use the same preloaded text, including empty sources.
Set `Parser.keep_source_texts` to keep parsed text in `FlatAst.source_texts`, keyed by file ID,
after parser buffers are released. The checker can index import lines from these snapshots.
The compiler driver preloads module-validation reads only when the module cache is disabled;
cache-enabled builds retain disk reads so parsing and cache signatures use the existing flow.

Bare builtin compile-time flags, such as `$if windows`, retain their compile-time meaning
even when a local variable has the same name. This applies to `$if` statements and expressions.
