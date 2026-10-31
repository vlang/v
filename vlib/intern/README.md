## Description

`intern` provides a string interning pool. `new_pool()` creates a pool and `pool.intern(text)`
returns a canonical string whose bytes are shared with every equal string in that pool.
The pool clones the first input, so callers can release their original storage. Treat returned
strings as immutable and do not free their bytes manually. `pool.len()` counts distinct strings.

`intern.eq(a, b)` first compares storage addresses, then compares values when needed. It also
works for strings from different pools and ordinary strings. Pools require external synchronization
when shared between threads.
