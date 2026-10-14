# Generic pointers in Option and Result returns

Generic factories can return `?&Box[T]` or `!&Box[T]`. The C backend keeps the pointer's
type declaration even when an unused function refers to a generic specialization that
does not otherwise need a complete struct definition. This also applies to imported
factories and pointer aliases, with both parallel and `-no-parallel` C generation.

Pointer Option and Result values preserve their ordinary `none` and error behavior.
Pointers to thread handles use the existing runtime handle type.
