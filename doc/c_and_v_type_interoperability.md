> [!NOTE]
> an important detail to remember, is that in V, the type always follows
> the identifier in declarations, i.e. you use `x int` in V in function parameters,
> and in struct declarations, not `int x`, like you would in C.

## Number types in C and V:

* A V `i8`  is equivalent to C `signed char`,  or `int8_t`.
* A V `i16` is equivalent to C `short`, or `int16_t`.
* A V `i32` is equivalent to C `int`,   or `int32_t`.
* A V `i64` is equivalent to C `long`,  or `int64_t`.

* A V `u8`  is equivalent to C `unsigned char`,  or `uint8_t`.
* A V `u16` is equivalent to C `unsigned short`, or `uint16_t`.
* A V `u32` is equivalent to C `unsigned int`,   or `uint32_t`.
* A V `u64` is equivalent to C `unsigned long`,  or `uint64_t`.

* A V `f32` is equivalent to C `float`.
* A V `f64` is equivalent to C `double`.

`char` in C is a distinct type with implementation-defined signedness, so use
V `char`, `&char`, or `charptr` when you need to match C `char`, `char*`,
or `char**`.

* A V `int` is a platform-width signed integer: 32 bits on 32-bit targets and
64 bits on 64-bit targets.

* A V `uint` is a platform-width unsigned integer, equivalent to C `size_t` or `uintptr_t`.
It is 32 bits on 32-bit targets and 64 bits on 64-bit targets.

`isize` and `usize` are deprecated. Use `int` and `uint`, respectively.
For C library wrappers, use `i32` for C `int` parameters, fields, and return values.
Plain `int` in a `fn C.` signature retains the C `int` ABI for compatibility.
Use `C.ptrdiff_t` for signed pointer-width parameters and return values, including C
`intptr_t`, `ptrdiff_t`, and `ssize_t`. It is an alias of V `int` that preserves the C ABI.

### C names for V's platform types

Generated C spells `int` as `i64` on 64-bit targets and `i32` on 32-bit ones, and
`uint` as `size_t`. C code included after the compiler's preamble can use the
target-independent names instead. Generated C defines them before ordinary quoted
`#include` and `#insert` directives:

| V type | C name | Defined in generated C as |
| --- | --- | --- |
| `int` | `__v_int` | `typedef i64 __v_int;` (64-bit), `typedef i32 __v_int;` (32-bit) |
| `uint` | `__v_uint` | `typedef size_t __v_uint;` |
| `isize` (deprecated) | `isize` | `typedef ptrdiff_t isize;` |
| `usize` (deprecated) | `usize` | `typedef size_t usize;` |

`__v_int` is a typedef of the type V already emits, so a C prototype spelled with it
has the same function type as the generated one on every target:

```c
__v_int get_count(__v_int id); // matches the generated `i64 get_count(i64 id)`
```

Place this declaration in a header included with quotes:

```v ignore
#include "v_counts.h"
```

`#preinclude` directives and angle-bracket headers such as `#include <v_counts.h>`
are emitted before the compiler's preamble, so they cannot use these generated
type names. Use standard C types in those early headers, or include a header that
uses V's generated names with quotes instead. This ordering lets early headers
configure the system libraries before V includes them.

Use `__v_uint` (or `size_t`) for V `uint`, which a `fn C.` signature also lowers to
`size_t`. The `isize` and `usize` typedefs remain for C code that already uses them,
independently of the deprecated V types of the same name.


## Representing pointers in C and V:
Pointers in V are equivalent to pointers in C in usage.
One difference in their declarations, is that the order of placing the pointer
sign is swapped. For example, you use `long *` in C, which is equivalent to `&i64` in V.

If you have a C function `func_name`, that accepts a `int *`, and an `int` index,
and returns an `unsigned char`:
```c ignore
unsigned char func_name(int * p, index int);
```
... then you can represent its function signature in V like this:

```v ignore
fn C.func_name(p &i32, index i32) u8
```

For a C output parameter, pass the address of the variable or struct field that C should update.
A cast such as `voidptr(&handle)` preserves that address when passed to a C `void **`
parameter declared as `&voidptr` in V. Writes by C update the original caller storage,
including fields in heap allocated structs.


## Representing compound types in C and V:
A V struct is the same as a C struct, with the same field names and
types, but the order of declaring field names and their types in C structs
and V structs is different - in C you use: `short field_name;`, but in V,
that would be: `field_name i16`.

V never parses or recursively expands C headers, including bundled headers. The C toolchain
handles preprocessing and include cycles. Redeclare the fields you use in a `.c.v` file,
and mark a C typedef with `@[typedef]`. For example,
`typedef struct { int x; } Foo;` needs this V binding:

```v ignore
@[typedef]
struct C.Foo {
	x i32
}
```

The attribute is required with both `#include` and `#insert`.

## Passing V strings to C functions:
The V string type, is currently defined like this:
```c
struct string {
    u8* str;
    int len;
    int is_lit;
};
```

If you have a string `s` in V, and you also have a C function which 
accepts `unsigned char *` in C, you can pass `s.str` to that function.
Do *not* pass `&s`, since that will be the address of the `s` struct
itself, and not the address of the characters in it (which is pointed
by `s.str`).


## Passing V dynamic array elements to C functions:
All V array types currently share the same C type, currently defined like this:
```c
struct array {
    void * data;
    // For arrays, the following `offset` field is 0.
    // For slices, the `offset` field contains the offset (in elements) from
    // the start of the original array.
    int offset; 
    int len;
    int cap;
    int flags;
    int element_size;
};
```									
									
If you want to pass a pointer to the elements of a V array `a`, to a C function,
use `a.data`. NOTE: do *not* use `&a` for that purpose. That will result in the address
of the `array` itself, getting passed to the C function, and *not* the address of the
elements of the V array.

Passing `a.data` to a V pointer parameter also passes the element buffer directly.
Parenthesizing the argument, such as `(a.data)`, preserves that behavior.
This holds when `a` is a pointer to an array or a mutable array parameter.


## Passing V fixed array elements to C functions:
Unlike dynamic arrays, fixed arrays are the same in both V and C.
The address of the first element of the V fixed array of integers `fa` with
type `[5]i32`, is `&fa[0]` . That address can be directly used a parameter to
C functions, that accept `int * p` or `int arr[]` parameters.


## Functions in V and C:
The type of a V function, with no receiver, is equivalent to a C function, 
given that the types of the parameters are equivalent.

Ordinary V functions named `sync` or `syncfs` can coexist with the system C functions.
Their generated C names also remain distinct from V functions named `main__sync` or `main__syncfs`.
Use the `C.` prefix to refer to the C function.
For Linux bindings that declare `fn C.syncfs`, V enables GNU header declarations before
including C headers, so `<unistd.h>` supplies the `syncfs` prototype.

When a V function returns more than one value, the C function returns a struct.
For example, these functions have equivalent types:
```v ignore
fn v_function(a u64, b i8) (i32, f32)
```
```c ignore
struct { int i; float f; } c_function(unsigned long a, signed char b);
````

A pointer to a V function is equivalent to a pointer to a C function, when the
functions have equivalent types.

For V functions taking `...voidptr`, non-pointer scalar arguments are stored in temporaries.
Integers and enum values narrower than the target's V `int` are widened to `int`, and `f32`
is widened to `f64`. The callee must read each slot using its corresponding storage type.


## Other V types, that are currently not supported/documented for use with C:
Other V types like interface, thread, chan, and map types are represented
as C structs, but they are deliberately not yet completely documented for the
purposes of C interoperability, since their implementations can still change.

C struct types containing bitfields, currently have no corresponding V types.

C++ class types have no corresponding V types.


## Memory allocation in V and C
Keep in mind, that V uses the Boehm's garbage collector by default. You can pass
a pointer to allocated memory from C to V, just remember that the responsibility
of freeing the pointer will remain with the C side, and if the C side frees the
pointer while the V side still has a copy, your program will fail.

When passing a pointer from V to C, the V function must retain a visible copy
of it in some V variable. Otherwise the V garbage collector may free the
corresponding memory, while the C function is still using it. You can use the
attribute `@[keep_args_alive]` to tag your `fn C.` declarations, that accept
pointers, to keep them from being freed, while the call to your `C.fname()`
function has not returned.
For more details, see also this
[test for keep_args_alive](vlib/v/slow_tests/keep_args_alive_test.c.v) .

When a module supplies a quoted header for a separate Objective-C++ source, the header owns
its native declarations. Use `@[c_extern]` on a V function binding when that header does not
provide a prototype. V does not inspect the header to infer which functions it declares.

With Objective-C ARC enabled, `isnil` compares native pointers directly, preserving their ownership.
