# V Work In Progress

***This document describes features that are not implemented, yet.
Please refer to [docs.md](https://github.com/vlang/v/blob/master/doc/docs.md)
for the current state of V***

## Table of Contents

* [Concurrency](#concurrency)
    * [Variable Declarations](#concurrency-variable-declarations)

## Concurrency

### Variable Declarations

Objects that are supposed to be used to exchange data between
coroutines have to be declared with special care. Exactly one of the following
4 kinds of declaration has to be chosen:

```v
a := ...
mut b := ...
shared c := ...
atomic d := ...
```

- `a` is declared as *constant* that can be passed to
  other coroutines and read without limitations. However
  it cannot be changed.
- `b` can be accessed reading and writing but only from one
  coroutine. That coroutine *owns* the object. A `mut` variable can
  be passed to another coroutine (as receiver or function argument in
  the `go` statement or via a channel) but then ownership is passed,
  too, and only the other coroutine can access the object.<sup>1</sup>
- `c` can be passed to coroutines an accessed
  *concurrently*.<sup>2</sup> In order to avoid data races it has to
  be locked before access can occur and unlocked to allow access to
  other coroutines. This is done by one the following block structures:
  ```v
  lock c {
      // read, modify, write c
      ...
  }
  ```
  
  ```v
  rlock c {
      // read c
      ...
  }
  ```
  Several variables may be specified: `lock x, y, z { ... }`.
  They are unlocked in the opposite order.
- `d` can be passed to coroutines and accessed *concurrently*,
  too.<sup>3</sup> No lock is needed in this case, however
  `atomic` variables can only be 32/64 bit integers (or pointers)
  and access is limited to a small set of predefined idioms that have
  native hardware support.

To help making the correct decision the following table summarizes the
different capabilities:

|                           | *default* | `mut` | `shared` | `atomic` |
| :---                      |   :---:   | :---: |  :---:   |  :---:   |
| write access              |           |   +   |     +    |    +     |
| concurrent access         |     +     |       |     +    |    +     |
| performance               |    ++     |  ++   |          |    +     |
| sophisticated operations  |     +     |   +   |     +    |          |
| structured data types     |     +     |   +   |     +    |          |

### Strengths
#### default
- very fast
- unlimited access from different coroutines
- easy to handle

#### `mut`
- very fast
- easy to handle

#### `shared`
- concurrent access from different coroutines
- data type may be complex structure
- sophisticated access possible (several statements within one `lock`
  block)

#### `atomic`
- concurrent access from different coroutines
- reasonably fast

### Weaknesses
#### default
- read only

#### `mut`
- access only from one coroutine at a time

#### `shared`
- lock/unlock are slow
- moderately difficult to handle (needs `lock` block)

#### `atomic`
- limited to single (max. 64 bit) integers (and pointers)
- only a small set of predefined operations possible
- very difficult to handle correctly

<sup>1</sup> The owning coroutine will also free the memory space used
for the object when it is no longer needed.  
<sup>2</sup> For `shared` objects the compiler adds code for reference
counting. Once the counter reaches 0 the object is automatically freed.  
<sup>3</sup> Since an `atomic` variable is only a few bytes in size
allocation would be an unnecessary overhead. Instead the compiler
creates a global.
