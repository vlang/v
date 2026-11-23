# Linux AMD64 backend

Build and run a Linux x86-64 executable with `./v -b amd64 run program.v`.
Add `-prod` before `run` to enable SSA optimization and register allocation.
The backend is experimental and currently uses no garbage collector.
Debug builds include native backtrace helpers.

Single-file programs that only print string literals use the shared native SSA reduced runtime.
Debug, profiling, and object builds retain the ordinary runtime.

Concrete generic sum types retain their variant tags and payload layouts, including recursive
pointer variants such as `Tree[T] = Empty | &Node[T]`. The binary search tree regression exercises
insertion, deletion, recursive methods, and floating-point values in normal, production, and
serial compilation modes:

```sh
./v vlib/v/gen/amd64/native_generic_sum_test.v
```

Custom errors returned as pointers preserve their concrete type and message when stored in
`IError`, including inside a Result. Test both native compilation modes with:

```sh
./v -b amd64 vlib/v/tests/result_custom_error_pointer_test.v
./v -prod -b amd64 vlib/v/tests/result_custom_error_pointer_test.v
```

See the native backend section in [the language documentation](../../../../doc/docs.md) for
platform and C interoperability restrictions.
