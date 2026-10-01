# Escaping binding storage

Taking a local value's address and retaining it extends that value's storage lifetime.
Bindings introduced by guards, multiple declarations, loops, and channel receives follow
the same rule as ordinary local declarations. Each loop iteration has its own retained
value and index bindings.

Shadowing introduces a separate binding. Initializers read the incoming bindings before
the new declarations become visible, and leaving a scope restores the outer bindings.

Closure value captures keep a snapshot of the value at capture time, including values
whose local storage has moved to the heap. Mutable value captures update that closure's
snapshot. Explicit pointer captures retain their pointer identity.
