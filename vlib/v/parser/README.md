# Parsing

Main-module functions cannot be named `dump`, `sizeof`, `typeof`, or `isreftype`.
These names have builtin syntax, so the compiler reports a redefinition error at the function
declaration. Methods, C declarations, and qualified functions in other modules can use these names.

Parenthesized expressions may span lines, including a line break before the closing `)`.
Line comments in a multiline condition preserve its grouping and any following `&&` or `||`.
