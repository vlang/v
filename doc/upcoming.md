# V Work In Progress

***This document describes features that are not implemented, yet.
Please refer to [docs.md](https://github.com/vlang/v/blob/master/doc/docs.md)
for the current state of V***

## Table of Contents

* [Concurrency](#concurrency)
    * [Variable Declarations](#variable-declarations)
	* [Strengths](#strengths)
	* [Weaknesses](#weaknesses)
	* [Compatibility](#compatibility)
	* [Automatic Lock](#automatic-lock)
	* [Channels](#channels)

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

### Compatibility
Outside of `lock`/`rlock` blocks function arguments must in general
match - with the familiar exception that objects declared `mut` can be
used to call functions expecting immutable arguments:

```v
fn f(x St) {...}
fn g(mut x St) {...}
fn h(shared x St) {...}
fn i(atomic x u64) {...}

a := St{...}
f(a)

mut b := &St{...} // reference since transferred to coroutine
f(b)
go g(mut b)
// `b` should not be accessed here any more

shared c := &St{...}
h(shared c)

atomic d &u64
i(atomic d)
```

Inside a `lock c {...}` block `c` behaves like a `mut`,
inside an `rlock c {...}` block like an immutable:
```v
shared c := &St{...}
lock c {
    g(mut c)
    f(c)
    // call to h() not allowed inside `lock` block
    // since h() will lock `c` itself
}
rlock c {
    f(c)
    // call to g() or h() not allowed
}
```

### Automatic Lock
In general the compiler will generate an error message when a `shared`
object is accessed outside of any corresponding `lock`/`rlock`
block. However in simple and obvious cases the necessary lock/unlock
can be generated automatically for `array`/`map` operations:

```v
shared a []int{...}
go h2(shared a)
a << 3
// keep in mind that `h2()` could change `a` between these statements
a << 4
x := a[1] // not necessarily `4`

shared b map[string]int
go h3(shared b)
b['apple'] = 3
c['plume'] = 7
y := b['apple'] // not necesarily `3`

// iteration over elements
for k, v in b {
    // concurrently changed k/v pairs may or my not be included
}
```

This is handy, but since other coroutines might access the `array`/`map`
concurrently between the automatically locked statements, the results
are sometimes surprising. Each statement should be seen as a single
transaction that is unrelated to the previous or following
statement. Therefore - but also for performance reasons - it's often
better to group consecutive coherent statements in an explicit `lock` block.

### Channels
Channels in V work basically like those in Go. You can `push()` objects into
a channel and `pop()` objects from a channel. They can be buffered or unbuffered
and it is possible to `select` from multiple channels.

#### Syntax and Usage
There is no support for channels in the core language (yet), so all functions
are in the `sync` library. Channels must be created as `mut` objects.

```v
mut ch := sync.new_channel<int>(0)    // unbuffered
mut ch2 := sync.new_channel<f64>(100) // buffer length 100
```

Channels can be passed to coroutines like normal `mut` variables:

```v
fn f(mut ch sync.Channel) {
    ...
}

fn main() {
    ...
    go f(mut ch)
    ...
}
```

The routines `push()` and `pop()` both use *references* to objects. This way
unnecessary copies of large objects are avoided and the call to `cannel_select()`
(see below) is simpler:

```v
n := 5
x := 7.3
ch.push(&n)
ch2.push(&x)

mut m := int(0)
mut y := f64(0.0)
ch.pop(&m)
ch2.pop(&y)
```

A channel can be closed to indicate that no further objects can be pushed. Any attempt
to do so will then result in a runtime panic. The `pop()` method will return immediately `false`
if the associated channel has been closed and the buffer is empty.

```v
ch.close()
...
if ch.pop(&m) {
    println('got $m')
} else {
    println('channel has been closed')
}
```

There are also methods `try_push()` and `try_pop()` which return immediatelly with the return value `.not_ready` if the transaction
cannot be performed without waiting. The return value is of type `sync.TransactionState` which can also be
`.success` or `.closed`.

To monitor a channel there is a method `len()` which returns the number of elements currently in the queue and the attribute
`cap` for the queue length. Please be aware that in general `channel.len() > 0` does not guarantee that the next
`pop()` will succeed without waiting, since other threads may already have "stolen" elements from the queue. Use `try_pop()` to
accomplish this kind of task.

The select call is somewhat tricky. The `channel_select()` function needs three arrays that
contain the channels, the directions (pop/push) and the object references and
a timeout of type `time.Duration` (`time.infinite` or `-1` to wait unlimited) as parameters. It returns the
index of the object that was pushed or popped or `-1` for timeout.

```v
mut chans := [ch, ch2]                    // the channels to monitor
directions := [sync.Direction.pop, .pop]  // .push or .pop
mut objs := [voidptr(&m), &y]             // the objects to push or pop

// idx contains the index of the object that was pushed or popped, -1 means timeout occured
idx := sync.channel_select(mut chans, directions, mut objs, 0) // wait unlimited
match idx {
    0 {
        println('got $m')
    }
    1 {
        println('got $y')
    }
    else {
        // idx = -1
        println('Timeout')
    }
}
```
If all channels have been closed `-2` is returned as index.
