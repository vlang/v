# Borrowed array storage

Borrowed array views share their source elements while a function reads or writes those
elements. Growing the buffer or removing owned elements first acquires independent
element owners. Operations that leave the buffer and elements unchanged keep borrowing.

Storing or returning a borrowed view with owned elements acquires independent owners.
Mutable array parameters also borrow their callers' owners when the buffer is managed and unsliced.
This includes map values, interface values, active sum variants, successful option and
result payloads, and closure value captures. A retained array reference receives a
separate header, so acquisition does not replace the caller's header.
Mutable parameters wrapped in options, results, or sums follow the same acquisition rule.

With ownership checking enabled, capturing a mutable array parameter by value snapshots
its elements for the closure. This also applies when the caller's array has an ordinary
managed buffer. Explicit pointer captures preserve their pointer identity.

Owned elements need a compatible `clone()` method when a nonempty borrow becomes owned.
Without one, retention fails with a diagnostic at runtime. Empty borrowed views can
become empty owned arrays without cloning an element.
