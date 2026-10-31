# Compile-time reflection sources

Reflection loops such as `$for variant in val.variants` resolve their source in the scope
of the function containing the loop. In a generic function, a parameter of type `T` can
be reflected on inside `$if T is $sumtype`. The compiler checks that parameter when its
function scope and type are available.

Importing a module with an unused generic reflection function does not resolve its
parameter as a type in another imported module. Named types used as reflection sources
are checked in the source file's own module, including before function body checking.

A concrete type passed to a generic reflection function keeps its declaring module.
For example, `config.Cfg` retains its own fields when another dependency imports
`rand.config`; an unrelated module import cannot change `$for field in T.fields`.

An explicit import alias keeps the type imported by that source file. A generic
reflection function can import another type under the same alias as its caller
without changing the concrete type passed as `T`.

Repeated method reflection loops in generic specializations borrow AST node headers during
metadata lookup. Empty method scans allocate only their metadata containers instead of a
node copy for every scanned AST entry.
