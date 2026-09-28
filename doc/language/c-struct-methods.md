# Methods on C structs

C struct receiver methods declared in directly imported V modules can be called through
C-valued fields and local copies. Full import paths determine visibility. A private extension
cannot hide an otherwise unambiguous public extension from another imported module.

Escaped method names such as `value.@union()` also work across imports, including generic
methods. Multiple visible public extensions with the same method name remain ambiguous.
Methods on V aliases take precedence over methods on their underlying C struct, including
methods inherited through a chain of aliases.
