# Methods on C structs

C struct receiver methods declared in directly imported V modules can be called through
C-valued fields and local copies. Full import paths determine visibility. A private extension
cannot hide an otherwise unambiguous public extension from another imported module.
An import used only to supply a resolved receiver method counts as used, including when
the method is bound as a callback.

Escaped method names such as `value.@union()` also work across imports, including generic
methods. Multiple visible public extensions with the same method name remain ambiguous.
Methods on V aliases take precedence over methods on their underlying C struct, including
methods inherited through a chain of aliases.

Visible receiver methods can also be bound as callbacks, for example `cb := value.read`.
Callbacks retain the same alias precedence and visibility rules as direct calls. Ambiguous
extensions are rejected even for method names such as `str`, `clone`, and `free` that have
compiler-provided defaults.
