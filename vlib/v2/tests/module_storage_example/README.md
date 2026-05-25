# Module Storage Example

This is a V2 fixture for module-scoped `__global` storage.

It is intentionally stored as `.vv2` source so shared V1/vfmt paths do not parse
V2-only public module-storage syntax directly. The regression test copies these
files to temporary `.v` files and compiles them with `-v2`.

Run:

```sh
./v test vlib/v2/types/module_storage_test.v
```
