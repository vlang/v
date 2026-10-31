# Borrowing through reflected methods

A concrete call produced by `$for method in T.methods` uses the same borrow and escape
information as an ordinary method call. Passing `&local` directly to `app.$method(&local)`
or assigning its scalar result to a local does not move the argument to the heap when the
method only borrows it. This includes structs containing inline fixed arrays.

A method that retains fixed-array storage or returns its pointer still requires storage that
outlives the caller. Unresolved calls remain conservative.
