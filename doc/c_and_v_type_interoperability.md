> [!NOTE]
> an important detail to remember, is that in V, the type always follows
> the identifier in declarations, i.e. you use `x int` in V in function parameters,
> and in struct declarations, not `int x`, like you would in C.

## Number types in C and V:

* A V `i8`  is equivalent to C `char`,  or `int8_t`.
* A V `i16` is equivalent to C `short`, or `int16_t`.
* A V `i32` is equivalent to C `int`,   or `int32_t`.
* A V `i64` is equivalent to C `long`,  or `int64_t`.

* A V `u8`  is equivalent to C `unsigned char`,  or `uint8_t`.
* A V `u16` is equivalent to C `unsigned short`, or `uint16_t`.
* A V `u32` is equivalent to C `unsigned int`,   or `uint32_t`.
* A V `u64` is equivalent to C `unsigned long`,  or `uint64_t`.

* A V `f32` is equivalent to C `float`.
* A V `f64` is equivalent to C `double`.

* A V `isize` is equivalent to C `ssize_t`.
It is a signed integer, and `isize` is guaranteed to be at least 16 bits.
It is 32 bit on 32 bit platforms, and 64 bits on 64 bit ones.

* A V `usize` is equivalent to C `size_t`.
It is an unsigned integer, and `usize` is guaranteed to be at least 16 bits.
It is 32 bit on 32 bit platforms, and 64 bits on 64 bit ones.

* A V `int` is currently equivalent to C `int`,  or `int64_t`.
There are plans to make it equivalent to `isize`, so if you want to write C
library wrappers, it is better to describe your `fn C.` parameters using `i32`
instead of using `int`.


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
fn C.func_name(p &int, index i32) u8
```


## Representing compound types in C and V:
A V struct is the same as a C struct, with the same field names and
types, but the order of declaring field names and their types in C structs
and V structs is different - in C you use: `short field_name;`, but in V,
that would be: `field_name i16`.

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


## Passing V fixed array elements to C functions:
Unlike dynamic arrays, fixed arrays are the same in both V and C.
The address of the first element of the V fixed array of integers `fa` with
type `[5]i32`, is `&fa[0]` . That address can be directly used a parameter to
C functions, that accept `int * p` or `int arr[]` parameters.


## Functions in V and C:
The type of a V function, with no receiver, is equivalent to a C function, 
given that the types of the parameters are equivalent.

When a V function returns more than one value, the C function returns a struct.
For example, these functions have equivalent types:
```v ignore
fn v_function(a u64, b i8) (i32, f32)
```
```c ignore
struct { int i; float f; } c_function(unsigned long a, char b);
````

A pointer to a V function is equivalent to a pointer to a C function, when the
functions have equivalent types.


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
