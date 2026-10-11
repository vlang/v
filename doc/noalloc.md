# Allocation contracts

`@[noalloc]` checks a V function and every function body reachable through its direct calls.
The compiler reports allocation sites with the root function, call chain, and source location.
Checks run during ordinary compilation and `-check`, including instantiated generic functions.
Allocations in startup code or unrelated functions do not violate the contract.

```v
@[noalloc]
fn factorial(n int) int {
	if n < 2 {
		return 1
	}
	return n * factorial(n - 1)
}
```

Checks cover source operations and the compiler's lowered code. Dynamic array and map creation,
string construction, interface and sum-type boxing, escaping local storage, closures, and
allocations inside called functions are rejected. An annotation on a V callee does not hide its
body: the compiler still checks it. Recursion is checked without revisiting the same body.

This check is conservative. Operations whose allocation behavior cannot be proved are rejected,
even when a particular execution would allocate nothing. This includes indirect function calls,
interface dispatch, bound method values, unexpanded compile-time code, custom operators, and
copies or returns of owning aggregates. The standalone FastC backend rejects V allocation contracts;
use the C backend. Contracts on function aliases and interface declarations are unsupported and
produce an error. Annotate V or C function declarations instead.

Automatic ownership and cleanup (`-ownership`, `-d ownership`, `-autofree`), profiling
(`-prof`, `-profile`), call tracing (`-trace-calls`), and coverage (`-coverage`) are rejected
for annotated functions. These modes can introduce callbacks or instrumentation after the
allocation checks, so their generated behavior cannot establish this contract.

## Reusable output buffers

By default, appending to an existing mutable array parameter is allowed. Such a buffer can grow
until it reaches its high-water mark. The exception covers only that buffer's growth: constructing
an appended dynamic array, slicing a fixed array, or boxing an interface element still fails.
Bulk append is rejected because an aliasing source can require a separate temporary copy.

```v
@[noalloc]
fn write_byte(mut out []u8, value u8) {
	out << value
}
```

Use `@[noalloc: strict]` to forbid buffer growth as well. Other attribute arguments are invalid.

## Foreign functions and termination

C declarations are assumed to allocate unless explicitly annotated with `@[noalloc]`. The
annotation is a trusted foreign contract, so the declaration's author must verify it. Standard
memory copying, comparison, searching, freeing, and string-length primitives have these contracts.

Calls to the known `panic`, `exit`, `C.exit`, and `C.abort` functions are exempt, including their
arguments, because they terminate execution. Statements before a terminating call remain checked;
an earlier branch that returns successfully cannot bypass the contract.
Hoisted argument allocations are exempt only for a source-validated body consisting solely of
the terminating call. More complex terminating control flow can be rejected conservatively.

Cached V declarations without an available body cannot establish a nonallocation proof. The
compiler rejects such calls rather than trusting an annotation in a cached declaration header.
