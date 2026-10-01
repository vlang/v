# Borrowed array storage

Borrowed array views share their source elements while a function reads or writes those
elements. Growing the buffer or removing owned elements first acquires independent
element owners. Operations that leave the buffer and elements unchanged keep borrowing.
Invalid growth requests report their builtin diagnostic before cloning borrowed elements.
Growth checks the available integer range before adding a requested amount to the size.
Trimming destroys removed elements only when the index is nonnegative and below the length.

Storing or returning a borrowed view with owned elements acquires independent owners.
Mutable array parameters also borrow their callers' owners when the buffer is managed and unsliced.
This includes map values, interface values, active sum variants, successful option and
result payloads, and closure value captures. A retained array reference receives a
separate header, so acquisition does not replace the caller's header.
Explicit option and result casts keep their wrappers when acquisition clones a successful payload.
Mutable parameters wrapped in options, results, or sums follow the same acquisition rule.
An active option or result array variant in a sum acquires its successful payload as well.
Internal Result wrapper acquisition clones a failed wrapper's boxed error owner.
Destructible custom errors need a compatible clone; failure to provide one rejects retention.
Compatible error clones may return a value or an independently owned pointer, including aliases.
Mutable sum copies also acquire independent boxes for their active by-value variants.

With ownership checking enabled, capturing a mutable array parameter by value snapshots
its elements for the closure. This also applies when the caller's array has an ordinary
managed buffer. Explicit pointer captures preserve their pointer identity.

Owned elements need a compatible `clone()` method when a nonempty borrow becomes owned.
Without one, retention fails with a diagnostic at runtime. Empty borrowed views can
become empty owned arrays without cloning an element.
