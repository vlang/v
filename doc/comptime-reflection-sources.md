# Compile-time reflection sources

Reflection loops such as `$for variant in val.variants` resolve their source in the scope
of the function containing the loop. In a generic function, a parameter of type `T` can
be reflected on inside `$if T is $sumtype`. The compiler checks that parameter when its
function scope and type are available.

Importing a module with an unused generic reflection function does not resolve its
parameter as a type in another imported module. Named types used as reflection sources
are checked in the source file's own module, including before function body checking.
