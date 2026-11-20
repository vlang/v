# Autofree semantics (working specification)

This document records the intended semantics of V's `-autofree` mode. It is a
specification for compiler work, not a promise that every rule is enforced yet.
`-autofree` is still WIP; the default memory manager remains the tracing GC.

## Model

`-autofree` is a compatibility lowering built on the ownership checker. It keeps
V's value-copy aliasing: copying a struct copies its pointer/length fields but
shares the underlying string, array or map buffers. Therefore a copy that shares
buffers with retained storage is a **borrow**, not a second owner. Exactly one
value is responsible for freeing a given allocation.

The ownership checker tracks three states per storage key:

* owned -- this value frees its storage at scope exit.
* moved -- ownership was transferred to another value.
* borrowed -- this value is a view; its root owner frees the storage.

`free()` (or `Drop` for custom types) is the destructor. The compiler inserts it
at the end of the owning value's lexical scope, on every exit path.

## Ownership and moves

* A fresh owning expression (literal, aggregate, call producing an owned value,
  explicit `.clone()`) creates a new owner.
* Passing or returning an owned value to a consumer transfers ownership. The
  source becomes moved and is no longer destroyed.
* A field or index projection read from storage that remains live is a borrow,
  not a move. It must not be destroyed.
* Scalars (`Copy` types) are trivially copied and never destroyed.

## Borrows and last use

A borrow ends at its **last use**, not necessarily at scope end. When a borrower
is no longer read before a conflicting write or move, its loan is dead and the
write/move is accepted. Ambiguous control flow (branches, loops, escaping
closures, deferred or spawned code, raw pointer aliases) keeps the loan alive
and the conservative diagnostic.

Borrows to array elements, map values and string slices keep their root owner.
`lent`-style accessors do not create owners. A `cursor` mark makes an edge
non-owning for graph structures.

## Cleanup at scope exit

Destructors run in reverse declaration order, at the end of the declaring
lexical scope. They run on normal exit, `return`, `break`, `continue`, and on
failed `?`/`!` propagation. A failed propagation destroys only locals that were
already initialized inside the scope where the failure occurred.

Nested blocks in deferred code keep their own cleanup scopes. A duplicated
lowerings of one source arm (a multi-type `match` branch) share the source
cleanup entries: each lowered variant frees the same locals, without adding new
cleanup sites.

## Interop and opt-outs

* `@[manualfree]` on a function, or `manualfree` as a global flag, disables all
  inserted cleanup for that body.
* C pointers are non-owning unless explicitly wrapped in an owning type.
* Custom `free()` methods are destructor hooks; `unsafe` must not turn a
  borrowed value into an owner.
* `defer` and `-gc none` retain their usual LIFO semantics and combine with
  autofree cleanup in declaration order.

## Known gaps

Full CFG-based liveness is not implemented; the current analysis is block-local
plus a conservative global check. Sum-type and interface payloads, closures,
and cycles need explicit `trace`-style handling. The strict per-implementer
`clone()` requirement for `IError` does not apply in autofree mode because error
values are shared.
