# Methods on C structs

C struct receiver methods declared in directly imported V modules can be called through
C-valued fields and local copies. Full import paths determine visibility. A private extension
cannot hide an otherwise unambiguous public extension from another imported module.
An import used only to supply a resolved receiver method counts as used, including when
the method is bound as a callback.
Calls with `if` or `match` arguments preserve the original storage of indexed `mut` and
reference receivers, and evaluate the receiver's index before those arguments.
The same visibility rules apply to imported `next()` methods used by `for ... in` loops
and imported `[]` and `[]=` index operators. Compound index updates can use a getter and
setter from separate imported modules.
Imported infix operators use these rules too, including compound updates and comparisons.
Static C declarations remain associated functions and do not become instance methods.
An imported value-receiver `hex()` method does not become a method on a pointer;
an explicit pointer-receiver declaration is required for that call. Ineligible value methods
do not make pointer calls ambiguous, including generic calls and method callbacks.

Escaped method names such as `value.@union()` also work across imports, including generic
methods. Multiple visible public extensions with the same method name remain ambiguous.
Methods on V aliases take precedence over methods on their underlying C struct, including
methods inherited through a chain of aliases. Aliases of pointers to C structs retain the same
lookup rules and pointer-receiver restrictions.

Visible receiver methods can also be bound as callbacks, for example `cb := value.read`.
Callbacks retain the same alias precedence and visibility rules as direct calls. Ambiguous
extensions are rejected even for method names such as `str`, `clone`, and `free` that have
compiler-provided defaults.
