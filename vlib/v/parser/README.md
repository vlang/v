# Parsing

Main-module functions cannot be named `dump`, `sizeof`, `typeof`, or `isreftype`.
These names have builtin syntax, so the compiler reports a redefinition error at the function
declaration. Methods, C declarations, and qualified functions in other modules can use these names.

Parenthesized expressions may span lines, including a line break before the closing `)`.
Line comments in a multiline condition preserve its grouping and any following `&&` or `||`.

A complete `if ... else` expression can supply an `if` condition, for example
`if if case_sensitive { a != b } else { a.to_lower() != b.to_lower() } { ... }`.
An accidental repeated `if` without an `else` still reports a parser error.

A struct initializer may put its opening `{` on the next line after the type name,
including qualified names and generic types. An explicit `;` still ends the preceding statement.
