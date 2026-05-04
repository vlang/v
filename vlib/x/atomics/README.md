# v-atomics
Low-level atomic operations for V with explicit i386 support (MMX required on i386).
Native atomic primitives for V implemented with inline assembly, without relying on C FFI.
This repository is an experiment in providing low-level atomic operations directly in V,
using V's inline assembly support.
At the moment, all operations provide sequentially consistent semantics.
## Motivation
In the current V ecosystem, atomic operations are implemented via calls into C.
While this approach works, it introduces an additional dependency on the C toolchain
and headers and limits control over the exact machine instructions being emitted.
x.atomics explores an alternative: **native atomic operations implemented directly in V**,
using architecture-specific inline assembly and explicit semantics.
The current focus of this project is:
- correctness of basic atomic primitives;
- predictable and inspectable code generation;
- sequentially consistent behavior for all operations.
In future versions, the set of supported atomic operations will be expanded,
and additional memory orderings will be introduced.
---
## Scope and Guarantees
- atomic operations on integer types implemented in V with inline assembly;
- architecture-specific implementations (per-platform `atomics.<arch>.v` files);
- **sequential consistency** for all exposed operations.
---
## Memory Model
All operations in this library are intended to be **sequentially consistent**:
- operations appear to be globally ordered;
- no weaker semantics (relaxed, acquire, release) are currently implemented;
- when weaker variants are added in the future, they will be explicitly named and documented.
---
## Examples
See the [examples](examples/) directory for complete runnable examples.
### Basic Usage
```v
import x.atomics

fn main() {
	mut value := i32(0)
	// Atomically store a value
	atomics.store_i32(&value, 42)
	// Atomically load the value
	loaded := atomics.load_i32(&value)
	// Atomic add: returns the new value after addition
	new_value := atomics.add_i32(&value, 10)
	// Atomic swap: returns the old value
	old := atomics.swap_i32(&value, 100)
	_ = loaded
	_ = new_value
	_ = old
}
```
### Compare-and-Swap (CAS)
```v
import x.atomics

fn main() {
	mut flag := u32(0)
	// CAS: if flag == 0, set it to 1; returns true on success
	if atomics.cas_u32(&flag, 0, 1) {
		println('Successfully changed flag from 0 to 1')
	}
}
```
### Bitwise Operations (AND / OR)
```
import x.atomics

fn main() {
	mut flags := u32(0xFF)
	// Atomically AND: clears lower nibble, returns old value
	old := atomics.and_u32(&flags, 0xF0)
	println('old: ${old}, new: ${flags}') // old: 255, new: 240
	// Atomically OR: sets a bit, returns old value
	prev := atomics.or_u32(&flags, 0x01)
	println('prev: ${prev}, new: ${flags}') // prev: 240, new: 241
}
```
### Available Operations
| Operation | i32 | i64 | u32 | u64 |
|-----------|-----|-----|-----|-----|
| `load_*`  | yes | yes | yes | yes |
| `store_*` | yes | yes | yes | yes |
| `add_*`   | yes | yes | yes | yes |
| `swap_*`  | yes | yes | yes | yes |
| `cas_*`   | yes | yes | yes | yes |
| `and_*`   | yes | yes | yes | yes |
| `or_*`    | yes | yes | yes | yes |
