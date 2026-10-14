# Nested storage when appending a local value

The default compiler can avoid recursively cloning nested storage when an array append
consumes an immutable local initialized by a proven tree factory or fresh literal in the same block.
The append must be the local's only use after initialization. A field path of that local
can qualify too, provided it does not traverse a pointer.

The optimization excludes mutable locals, aliases initialized from another variable, and
locals read before or after the append. It also excludes appends separated from the
initializer by loops, closures, deferred work, spawned work, or jumps.

The compiler proves factories before parallel transformation. They must take scalar inputs,
read only their parameters and locals, and consume each storage-bearing local at most once.
Loops, indirect or foreign calls, and borrowed storage exclude a factory. Recursive factories
can qualify, allowing bottom-up tree construction to reuse a finished child's storage.

Calls that can return an argument's storage, literals containing borrowed fields, factories
with shared fields, and types with explicit clone methods keep their existing clones.
It also preserves the existing cloning of mutable or reused values. Array literals and bulk
appends retain their existing storage behavior; this optimization does not make those forms
and single-element appends share one copying rule.
