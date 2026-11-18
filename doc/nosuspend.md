# Suspension contracts

`@[nosuspend]` checks a V function and every function body reachable through
its direct calls. The compiler reports suspension points with the root
function, call chain, and source location. Checks run during ordinary
compilation and `-check`, including instantiated generic functions.
Suspension points in startup code or unrelated functions do not violate
the contract.

```v
@[nosuspend]
fn update_state(n int) int {
	return n + 1
}
```

Checks cover `spawn`, channel send and receive (`<-`, `chan <-`), `select`,
`lock`/`rlock`, and known blocking calls such as `time.sleep`,
`sync.Mutex.lock`, `sync.RwMutex.lock`, `sync.WaitGroup.wait`,
`sync.Barrier.wait`, `sync.Semaphore.wait`, `sync.Cond.wait`, and blocking
OS or network IO reached through `C`. Allocations, string building, map use,
and other non-suspending operations remain allowed. An annotation on a V
callee does not hide its body: the compiler still checks it. Cached imports
are read from source when their bodies are needed for suspension checks.
Recursion is checked without revisiting the same body.

This check is conservative. Operations whose suspension behavior cannot be
proved are rejected, even when a particular execution would not block. This
includes indirect function calls, interface dispatch, bound method values,
unexpanded compile-time code, and calls without an analyzable function body
or `C` contract. The standalone FastC backend rejects V suspension
contracts; use the C backend. Contracts on function aliases and interface
declarations are unsupported and produce an error. Annotate V or C function
declarations instead.

## Foreign functions and termination

C declarations are assumed to suspend unless explicitly annotated with
`@[nosuspend]`. The annotation is a trusted foreign contract, so the
declaration author must verify it. Standard non-blocking memory copying,
comparison, searching, freeing, and string-length primitives carry these
contracts.

Calls to the known `panic`, `exit`, `C.exit`, and `C.abort` functions are
exempt, including their arguments handling for termination, because they
terminate execution. Statements before a terminating call remain checked.
The exemption applies only to the actual `builtin`, `os`, and `C`
declarations: local bindings or struct fields that spell the same name are
still rejected as indirect calls.
