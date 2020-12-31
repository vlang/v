# V Documentation

## Introduction

V is a statically typed compiled programming language designed for building maintainable software.

It's similar to Go and its design has also been influenced by Oberon, Rust, Swift,
Kotlin, and Python.

V is a very simple language. Going through this documentation will take you about an hour,
and by the end of it you will have pretty much learned the entire language.

The language promotes writing simple and clear code with minimal abstraction.

Despite being simple, V gives the developer a lot of power.
Anything you can do in other languages, you can do in V.

## Table of Contents

<table>
    <tr><td width=33% valign=top>

* [Hello world](#hello-world)
* [Comments](#comments)
* [Functions](#functions)
    * [Returning multiple values](#returning-multiple-values)
    * [Variable number of arguments](#variable-number-of-arguments)
* [Symbol visibility](#symbol-visibility)
* [Variables](#variables)
* [Types](#types)
    * [Strings](#strings)
    * [Numbers](#numbers)
    * [Arrays](#arrays)
    * [Maps](#maps)
* [Module imports](#module-imports)
* [Statements & expressions](#statements--expressions)
    * [If](#if)
    * [In operator](#in-operator)
    * [For loop](#for-loop)
    * [Match](#match)
    * [Defer](#defer)
* [Structs](#structs)
    * [Embedded structs](#embedded-structs)
    * [Default field values](#default-field-values)
    * [Short struct literal syntax](#short-struct-initialization-syntax)
    * [Access modifiers](#access-modifiers)
    * [Methods](#methods)

</td><td width=33% valign=top>

* [Functions 2](#functions-2)
    * [Pure functions by default](#pure-functions-by-default)
    * [Mutable arguments](#mutable-arguments)
    * [Anonymous & high order functions](#anonymous--high-order-functions)
* [References](#references)
* [Modules](#modules)
* [Constants](#constants)
* [Builtin functions](#builtin-functions)
* [Printing custom types](#printing-custom-types)
* [Types 2](#types-2)
    * [Interfaces](#interfaces)
    * [Enums](#enums)
    * [Sum types](#sum-types)
    * [Option/Result types & error handling](#optionresult-types-and-error-handling)
* [Generics](#generics)
* [Concurrency](#concurrency)
    * [Channels](#channels)
* [Decoding JSON](#decoding-json)
* [Testing](#testing)
* [Memory management](#memory-management)
* [ORM](#orm)

</td><td valign=top>

* [Writing documentation](#writing-documentation)
* [Tools](#tools)
    * [v fmt](#v-fmt)
    * [Profiling](#profiling)
* [Advanced Topics](#advanced-topics)
    * [Memory-unsafe code](#memory-unsafe-code)
    * [Calling C functions from V](#calling-c-functions-from-v)
    * [Debugging generated C code](#debugging-generated-c-code)
    * [Conditional compilation](#conditional-compilation)
    * [Compile time pseudo variables](#compile-time-pseudo-variables)
    * [Compile-time reflection](#compile-time-reflection)
    * [Limited operator overloading](#limited-operator-overloading)
    * [Inline assembly](#inline-assembly)
    * [Translating C to V](#translating-c-to-v)
    * [Hot code reloading](#hot-code-reloading)
    * [Cross compilation](#cross-compilation)
    * [Cross-platform shell scripts in V](#cross-platform-shell-scripts-in-v)
    * [Attributes](#attributes)
* [Appendices](#appendices)
    * [Keywords](#appendix-i-keywords)
    * [Operators](#appendix-ii-operators)

</td></tr>
</table>

<!--
There are several special keywords, which you can put after the code fences for v.
These are:
   compile      - default, you do not need to specify it. cmd/tools/check-md.v compile the example.
   ignore       - ignore the example, useful for examples that just use the syntax highlighting
   failcompile  - known failing compilation. Useful for examples demonstrating compiler errors.
   oksyntax     - it should parse, it may not compile. Useful for partial examples.
   badsyntax    - known bad syntax, it should not even parse
   wip          - like ignore; a planned feature; easy to search.
-->

## Hello World


```v
fn main() {
	println('hello world')
}
```

Save this snippet into a file named `hello.v`. Now do: `v run hello.v`.

> That is assuming you have symlinked your V with `v symlink`, as described
[here](https://github.com/vlang/v/blob/master/README.md#symlinking).
If you haven't yet, you have to type the path to V manually.

Congratulations - you just wrote and executed your first V program!

You can compile a program without execution with `v hello.v`.
See `v help` for all supported commands.

From the example above, you can see that functions are declared with the `fn` keyword.
The return type is specified after the function name.
In this case `main` doesn't return anything, so there is no return type.

As in many other languages (such as C, Go, and Rust), `main` is the entry point of your program.

`println` is one of the few built-in functions.
It prints the value passed to it to standard output.

`fn main()` declaration can be skipped in one file programs.
This is useful when writing small programs, "scripts", or just learning the language.
For brevity, `fn main()` will be skipped in this tutorial.

This means that a "hello world" program in V is as simple as

```v
println('hello world')
```

## Comments

```v
// This is a single line comment.
/*
This is a multiline comment.
   /* It can be nested. */
*/
```

## Functions

```v
fn main() {
	println(add(77, 33))
	println(sub(100, 50))
}

fn add(x int, y int) int {
	return x + y
}

fn sub(x int, y int) int {
	return x - y
}
```

Again, the type comes after the argument's name.

Just like in Go and C, functions cannot be overloaded.
This simplifies the code and improves maintainability and readability.

Functions can be used before their declaration:
`add` and `sub` are declared after `main`, but can still be called from `main`.
This is true for all declarations in V and eliminates the need for header files
or thinking about the order of files and declarations.

### Returning multiple values

```v nofmt
fn foo() (int, int) {
    return 2, 3
}

a, b := foo()
println(a) // 2
println(b) // 3
c, _ := foo() // ignore values using `_`
```

### Variable number of arguments

```v nofmt
fn sum(a ...int) int {
    mut total := 0
    for x in a {
        total += x
    }
    return total
}
println(sum())    // Output: 0
println(sum(1))   //         1
println(sum(2,3)) //         5

// using array decomposition
a := [2,3,4]
println(sum(a...))  // <-- using postfix ... here. output: 9
b := [5, 6, 7]
println(sum(b...)) // output: 18
```

## Symbol visibility

```v
pub fn public_function() {
}

fn private_function() {
}
```

Functions are private (not exported) by default.
To allow other modules to use them, prepend `pub`. The same applies
to constants and types.

Note: `pub` can only be used from a named module.
For information about creating a module, see [Modules](#modules).

## Variables

```v
name := 'Bob'
age := 20
large_number := i64(9999999999)
println(name)
println(age)
println(large_number)
```

Variables are declared and initialized with `:=`. This is the only
way to declare variables in V. This means that variables always have an initial
value.

The variable's type is inferred from the value on the right hand side.
To choose a different type, use type conversion:
the expression `T(v)` converts the value `v` to the
type `T`.

Unlike most other languages, V only allows defining variables in functions.
Global (module level) variables are not allowed. There's no global state in V
(see [Pure functions by default](#pure-functions-by-default) for details).

For consistency across different code bases, all variable and function names
must use the `snake_case` style, as opposed to type names, which must use `PascalCase`.

### Mutable variables

```v
mut age := 20
println(age)
age = 21
println(age)
```

To change the value of the variable use `=`. In V, variables are
immutable by default.
To be able to change the value of the variable, you have to declare it with `mut`.

Try compiling the program above after removing `mut` from the first line.

### Initialization vs assignment

Note the (important) difference between `:=` and `=`.
`:=` is used for declaring and initializing, `=` is used for assigning.

```v failcompile
fn main() {
    age = 21
}
```

This code will not compile, because the variable `age` is not declared.
All variables need to be declared in V.

```v
fn main() {
	age := 21
}
```

### Declaration errors

In development mode the compiler will warn you that you haven't used the variable
(you'll get an "unused variable" warning).
In production mode (enabled by passing the `-prod` flag to v – `v -prod foo.v`)
it will not compile at all (like in Go).

```v failcompile
fn main() {
    a := 10
    if true {
        a := 20 // error: redefinition of `a`
    }
    // warning: unused variable `a`
}
```

Unlike most languages, variable shadowing is not allowed. Declaring a variable with a name
that is already used in a parent scope will cause a compilation error.

You can shadow imported modules though, as it is very useful in some situations:
```v ignore
import ui
import gg

fn draw(ctx &gg.Context) {
    gg := ctx.parent.get_ui().gg
    gg.draw_rect(10, 10, 100, 50)
}
```

## Types

### Primitive types

```v ignore
bool

string

i8    i16  int  i64      i128 (soon)
byte  u16  u32  u64      u128 (soon)

rune // represents a Unicode code point

f32 f64

any_int, any_float // internal intermediate types of number literals

byteptr, voidptr, charptr, size_t // these are mostly used for C interoperability

any // similar to C's void* and Go's interface{}
```

Please note that unlike C and Go, `int` is always a 32 bit integer.

There is an exception to the rule that all operators
in V must have values of the same type on both sides. A small primitive type
on one side can be automatically promoted if it fits
completely into the data range of the type on the other side.
These are the allowed possibilities:

```v ignore
   i8 → i16 → int → i64
                  ↘     ↘
                    f32 → f64
                  ↗     ↗
 byte → u16 → u32 → u64 ⬎
      ↘     ↘     ↘      ptr
   i8 → i16 → int → i64 ⬏
```
An `int` value for example can be automatically promoted to `f64`
or `i64` but not to `f32` or `u32`. (`f32` would mean precision
loss for large values and `u32` would mean loss of the sign for
negative values).

### Strings

```v nofmt
name := 'Bob'
println(name.len)
println(name[0]) // indexing gives a byte B
println(name[1..3]) // slicing gives a string 'ob'

windows_newline := '\r\n' // escape special characters like in C
assert windows_newline.len == 2
```

In V, a string is a read-only array of bytes. String data is encoded using UTF-8.
String values are immutable. You cannot mutate elements:

```v failcompile
mut s := 'hello 🌎'
s[0] = `H` // not allowed
```
> error: cannot assign to `s[i]` since V strings are immutable

Note that indexing a string will produce a `byte`, not a `rune`. Indexes correspond
to bytes in the string, not Unicode code points.

Character literals have type `rune`. To denote them, use `

```v
rocket := `🚀`
assert 'aloha!'[0] == `a`
```

Both single and double quotes can be used to denote strings. For consistency,
`vfmt` converts double quotes to single quotes unless the string contains a single quote character.

For raw strings, prepend `r`. Raw strings are not escaped:

```v nofmt
s := r'hello\nworld'
println(s) // "hello\nworld"
```

### String interpolation

Basic interpolation syntax is pretty simple - use `$` before a variable name.
The variable will be converted to a string and embedded into the literal:
```v nofmt
name := 'Bob'
println('Hello, $name!') // Hello, Bob!
```
It also works with fields: `'age = $user.age'`.
If you need more complex expressions, use `${}`: `'can register = ${user.age > 13}'`.

Format specifiers similar to those in C's `printf()` are also supported.
`f`, `g`, `x`, etc. are optional and specify the output format.
The compiler takes care of the storage size, so there is no `hd` or `llu`.

```v nofmt
x := 123.4567
println('x = ${x:4.2f}')
println('[${x:10}]')       // pad with spaces on the left
println('[${int(x):-10}]') // pad with spaces on the right
```

### String operators

```v nofmt
name := 'Bob'
bobby := name + 'by' // + is used to concatenate strings
println(bobby) // "Bobby"

mut s := 'hello '
s += 'world' // `+=` is used to append to a string
println(s) // "hello world"
```
All operators in V must have values of the same type on both sides.
You cannot concatenate an integer to a string:

```v failcompile
age := 10
println('age = ' + age) // not allowed
```
> error: infix expr: cannot use `int` (right expression) as `string`

We have to either convert `age` to a `string`:

```v
age := 11
println('age = ' + age.str())
```

or use string interpolation (preferred):

```v
age := 12
println('age = $age')
```

### Numbers

```v
a := 123
```

This will assign the value of 123 to `a`. By default `a` will have the
type `int`.

You can also use hexadecimal, binary or octal notation for integer literals:

```v
a := 0x7B
b := 0b01111011
c := 0o173
```

All of these will be assigned the same value, 123. They will all have type
`int`, no matter what notation you used.

V also supports writing numbers with `_` as separator:

```v nofmt
num := 1_000_000 // same as 1000000
three := 0b0_11 // same as 0b11
float_num := 3_122.55  // same as 3122.55
hexa := 0xF_F // same as 255
oct := 0o17_3 // same as 0o173
```

If you want a different type of integer, you can use casting:

```v
a := i64(123)
b := byte(42)
c := i16(12345)
```

Assigning floating point numbers works the same way:

```v
f := 1.0
f1 := f64(3.14)
f2 := f32(3.14)
```
If you do not specify the type explicitly, by default float literals
will have the type of `f64`.

### Arrays

```v nofmt
mut nums := [1, 2, 3]
println(nums) // "[1, 2, 3]"
println(nums[1]) // "2"
nums[1] = 5
println(nums) // "[1, 5, 3]"

println(nums[0..2]) // slicing gives an array "[1, 5]"

println(nums.len) // "3"
nums = [] // The array is now empty
println(nums.len) // "0"

// Declare an empty array:
users := []int{}
```

The type of an array is determined by the first element:
* `[1, 2, 3]` is an array of ints (`[]int`).
* `['a', 'b']` is an array of strings (`[]string`).

The user can explicitly specify the type for the first element: `[byte(16), 32, 64, 128]`.
V arrays are homogeneous (all elements must have the same type).
This means that code like `[1, 'a']` will not compile.

The `.len` field returns the length of the array. Note that it's a read-only field,
and it can't be modified by the user. Exported fields are read-only by default in V.
See [Access modifiers](#access-modifiers).

#### Array operations

```v nofmt
mut nums := [1, 2, 3]
nums << 4
println(nums) // "[1, 2, 3, 4]"

// append array
nums << [5, 6, 7]
println(nums) // "[1, 2, 3, 4, 5, 6, 7]"

mut names := ['John']
names << 'Peter'
names << 'Sam'
// names << 10  <-- This will not compile. `names` is an array of strings.
println(names.len) // "3"
println('Alex' in names) // "false"
```

`<<` is an operator that appends a value to the end of the array.
It can also append an entire array.

`val in array` returns true if the array contains `val`. See [`in` operator](#in-operator).

#### Initializing array properties

During initialization you can specify the capacity of the array (`cap`), its initial length (`len`),
and the default element (`init`):

```v
arr := []int{len: 5, init: -1}
// `[-1, -1, -1, -1, -1]`
```

Setting the capacity improves performance of insertions,
as it reduces the number of reallocations needed:

```v nofmt
mut numbers := []int{ cap: 1000 }
println(numbers.len) // 0
// Now appending elements won't reallocate
for i in 0 .. 1000 {
    numbers << i
}
```
Note: The above code uses a [range `for`](#range-for) statement.

#### Array methods

All arrays can be easily printed with `println(arr)` and converted to a string
with `s := arr.str()`.

Copying the data from the array is done with `.clone()`:

```v nofmt
nums := [1, 2, 3]
nums_copy := nums.clone()
```

Arrays can be efficiently filtered and mapped with the `.filter()` and
`.map()` methods:

```v nofmt
nums := [1, 2, 3, 4, 5, 6]
even := nums.filter(it % 2 == 0)
println(even) // [2, 4, 6]
// filter can accept anonymous functions
even_fn := nums.filter(fn (x int) bool {
	return x % 2 == 0
})
println(even_fn)

words := ['hello', 'world']
upper := words.map(it.to_upper())
println(upper) // ['HELLO', 'WORLD']
// map can also accept anonymous functions
upper_fn := words.map(fn (w string) string {
	return w.to_upper()
})
println(upper_fn) // ['HELLO', 'WORLD']
```

`it` is a builtin variable which refers to element currently being processed in filter/map methods.

#### Multidimensional Arrays

Arrays can have more than one dimension.

2d array example:
```v nofmt
mut a := [][]int{len:2, init: []int{len:3}}
a[0][1] = 2
println(a) // [[0, 2, 0], [0, 0, 0]]
```

3d array example:
```v nofmt
mut a := [][][]int{len:2, init: [][]int{len:3, init: []int{len:2}}}
a[0][1][1] = 2
println(a) // [[[0, 0], [0, 2], [0, 0]], [[0, 0], [0, 0], [0, 0]]]
```

#### Sorting arrays

Sorting arrays of all kinds is very simple and intuitive. Special variables `a` and `b`
are used when providing a custom sorting condition.

```v nofmt
mut numbers := [1, 3, 2]
numbers.sort()      // 1, 2, 3
numbers.sort(a > b) // 3, 2, 1
```

```v nofmt
struct User { age int  name string }
mut users := [User{21, 'Bob'}, User{20, 'Zarkon'}, User{25, 'Alice'}]
users.sort(a.age < b.age)   // sort by User.age int field
users.sort(a.name > b.name) // reverse sort by User.name string field
```

### Maps

```v nofmt
mut m := map[string]int // Only maps with string keys are allowed for now
m['one'] = 1
m['two'] = 2
println(m['one']) // "1"
println(m['bad_key']) // "0"
println('bad_key' in m) // Use `in` to detect whether such key exists
m.delete('two')

// Short syntax
numbers := {
    'one': 1
    'two': 2
}
```

## Module imports

For information about creating a module, see [Modules](#modules).

Modules can be imported using the `import` keyword:

```v
import os

fn main() {
	// read text from stdin
	name := os.input('Enter your name: ')
	println('Hello, $name!')
}
```
This program can use any public definitions from the `os` module, such
as the `input` function. See the [standard library](https://modules.vlang.io/)
documentation for a list of common modules and their public symbols.

By default, you have to specify the module prefix every time you call an external function.
This may seem verbose at first, but it makes code much more readable
and easier to understand - it's always clear which function from
which module is being called. This is especially useful in large code bases.

### Selective imports

You can also import specific functions and types from modules directly:

```v nofmt
import os { input }
import crypto.sha256 { sum }
import time { Time }
```
Note: This is not allowed for constants - they must always be prefixed.

You can import several specific symbols at once:

```v
import os { input, user_os }

name := input('Enter your name: ')
println('Name: $name')
os := user_os()
println('Your OS is ${os}.')
```

### Module import aliasing

Any imported module name can be aliased using the `as` keyword:

NOTE: this example will not compile unless you have created `mymod/sha256.v`
```v failcompile
import crypto.sha256
import mymod.sha256 as mysha256

fn main() {
    v_hash := sha256.sum('hi'.bytes()).hex()
    my_hash := mysha256.sum('hi'.bytes()).hex()
    assert my_hash == v_hash
}
```

You cannot alias an imported function or type.
However, you _can_ redeclare a type.

```v
import time
import math

type MyTime = time.Time

fn (mut t MyTime) century() int {
	return int(1.0 + math.trunc(f64(t.year) * 0.009999794661191))
}

fn main() {
	mut my_time := MyTime{
		year: 2020
		month: 12
		day: 25
	}
	println(time.new_time(my_time).utc_string())
	println('Century: $my_time.century()')
}
```

## Statements & expressions

### If

```v
a := 10
b := 20
if a < b {
	println('$a < $b')
} else if a > b {
	println('$a > $b')
} else {
	println('$a == $b')
}
```

`if` statements are pretty straightforward and similar to most other languages.
Unlike other C-like languages,
there are no parentheses surrounding the condition and the braces are always required.

`if` can be used as an expression:

```v
num := 777
s := if num % 2 == 0 { 'even' } else { 'odd' }
println(s)
// "odd"
```

#### Type checks and casts
You can check the current type of a sum type using `is` and its negated form `!is`.

You can do it either in an `if`:
```v nofmt
struct Abc {
    val string
}
struct Xyz {
    foo string
}
type Alphabet = Abc | Xyz

x := Alphabet(Abc{'test'}) // sum type
if x is Abc {
    // x is automatically casted to Abc and can be used here
    println(x)
}
if x !is Abc {
    println('Not Abc')
}
```
or using `match`:
```v oksyntax
match x {
	Abc {
		// x is automatically casted to Abc and can be used here
		println(x)
	}
	Xyz {
		// x is automatically casted to Xyz and can be used here
		println(x)
	}
}
```

This works also with struct fields:
```v
struct MyStruct {
	x int
}

struct MyStruct2 {
	y string
}

type MySumType = MyStruct | MyStruct2

struct Abc {
	bar MySumType
}

x := Abc{
	bar: MyStruct{123} // MyStruct will be converted to MySumType type automatically
}
if x.bar is MyStruct {
	// x.bar is automatically casted
	println(x.bar)
}
match x.bar {
	MyStruct {
		// x.bar is automatically casted
		println(x.bar)
	}
	else {}
}
```

Mutable variables can change, and doing a cast would be unsafe.
However, sometimes it's needed to have a type cast despite of mutability.
In this case the developer has to mark the expression with a `mut` keyword
to tell the compiler that you're aware of what you're doing.

It works like this:
```v oksyntax
mut x := MySumType(MyStruct{123})
if mut x is MyStruct {
	// x is casted to MyStruct even it's mutable
	// without the mut keyword that wouldn't work
	println(x)
}
// same with match
match mut x {
	MyStruct {
		// x is casted to MyStruct even it's mutable
		// without the mut keyword that wouldn't work
		println(x)
	}
}
```

### In operator

`in` allows to check whether an array or a map contains an element.

```v nofmt
nums := [1, 2, 3]
println(1 in nums) // true

m := {'one': 1, 'two': 2}
println('one' in m) // true
```

It's also useful for writing boolean expressions that are clearer and more compact:

```v
enum Token {
	plus
	minus
	div
	mult
}

struct Parser {
	token Token
}

parser := Parser{}
if parser.token == .plus || parser.token == .minus || parser.token == .div || parser.token == .mult {
	// ...
}
if parser.token in [.plus, .minus, .div, .mult] {
	// ...
}
```

V optimizes such expressions,
so both `if` statements above produce the same machine code and no arrays are created.

### For loop

V has only one looping keyword: `for`, with several forms.

#### Array `for`

```v nofmt
numbers := [1, 2, 3, 4, 5]
for num in numbers {
    println(num)
}
names := ['Sam', 'Peter']
for i, name in names {
    println('$i) $name')  // Output: 0) Sam
}                         //         1) Peter
```

The `for value in arr` form is used for going through elements of an array.
If an index is required, an alternative form `for index, value in arr` can be used.

Note, that the value is read-only.
If you need to modify the array while looping, you have to use indexing:

```v nofmt
mut numbers := [0, 1, 2]
for i, _ in numbers {
	numbers[i]++
}
println(numbers) // [1, 2, 3]
```
When an identifier is just a single underscore, it is ignored.

#### Map `for`

```v nofmt
m := {'one':1, 'two':2}
for key, value in m {
    println("$key -> $value")  // Output: one -> 1
}                              //         two -> 2
```

Either key or value can be ignored by using a single underscore as the identifier.
```v nofmt
m := {'one':1, 'two':2}

// iterate over keys
for key, _ in m {
    println(key)  // Output: one
}                 //         two

// iterate over values
for _, value in m {
    println(value)  // Output: 1
}                   //         2
```

#### Range `for`

```v
// Prints '01234'
for i in 0 .. 5 {
	print(i)
}
```
`low..high` means an *exclusive* range, which represents all values
from `low` up to *but not including* `high`.

#### Condition `for`

```v nofmt
mut sum := 0
mut i := 0
for i <= 100 {
	sum += i
	i++
}
println(sum) // "5050"
```

This form of the loop is similar to `while` loops in other languages.
The loop will stop iterating once the boolean condition evaluates to false.
Again, there are no parentheses surrounding the condition, and the braces are always required.

#### Bare `for`

```v nofmt
mut num := 0
for {
	num += 2
	if num >= 10 {
		break
	}
}
println(num) // "10"
```

The condition can be omitted, resulting in an infinite loop.

#### C `for`

```v
for i := 0; i < 10; i += 2 {
	// Don't print 6
	if i == 6 {
		continue
	}
	println(i)
}
```

Finally, there's the traditional C style `for` loop. It's safer than the `while` form
because with the latter it's easy to forget to update the counter and get
stuck in an infinite loop.

Here `i` doesn't need to be declared with `mut` since it's always going to be mutable by definition.

#### Labelled break & continue

`break` and `continue` control the innermost `for` loop by default.
You can also use `break` and `continue` followed by a label name to refer to an outer `for`
loop:

```v
outer: for i := 4; true; i++ {
	println(i)
	for {
		if i < 7 {
			continue outer
		} else {
			break outer
		}
	}
}
```
The label must immediately precede the outer loop.
The above code prints:
```
4
5
6
7
```

### Match

```v
os := 'windows'
print('V is running on ')
match os {
	'darwin' { println('macOS.') }
	'linux' { println('Linux.') }
	else { println(os) }
}
```

A match statement is a shorter way to write a sequence of `if - else` statements.
When a matching branch is found, the following statement block will be run.
The else branch will be run when no other branches match.

```v
number := 2
s := match number {
	1 { 'one' }
	2 { 'two' }
	else { 'many' }
}
```

A match expression returns the value of the final expression from the matching branch.

```v
enum Color {
	red
	blue
	green
}

fn is_red_or_blue(c Color) bool {
	return match c {
		.red, .blue { true } // comma can be used to test multiple values
		.green { false }
	}
}
```

A match statement can also be used to branch on the variants of an `enum`
by using the shorthand `.variant_here` syntax. An `else` branch is not allowed
when all the branches are exhaustive.

```v
c := `v`
typ := match c {
	`0`...`9` { 'digit' }
	`A`...`Z` { 'uppercase' }
	`a`...`z` { 'lowercase' }
	else { 'other' }
}
println(typ)
// 'lowercase'
```

You can also use ranges as `match` patterns. If the value falls within the range
of a branch, that branch will be executed.

Note that the ranges use `...` (three dots) rather than `..` (two dots). This is
because the range is *inclusive* of the last element, rather than exclusive
(as `..` ranges are). Using `..` in a match branch will throw an error.

Note: `match` as an expression is not usable in `for` loop and `if` statements.

### Defer

A defer statement defers the execution of a block of statements
until the surrounding function returns.

```v
import os

fn read_log() {
	mut ok := false
	mut f := os.open('log.txt') or { panic(err) }
	defer {
		f.close()
	}
	// ...
	if !ok {
		// defer statement will be called here, the file will be closed
		return
	}
	// ...
	// defer statement will be called here, the file will be closed
}
```

## Structs

```v nofmt
struct Point {
    x int
    y int
}

mut p := Point{
    x: 10
    y: 20
}

println(p.x) // Struct fields are accessed using a dot

// Alternative literal syntax for structs with 3 fields or fewer
p = Point{10, 20}
assert p.x == 10
```

### Heap structs

Structs are allocated on the stack. To allocate a struct on the heap
and get a reference to it, use the `&` prefix:

```v
struct Point {
	x int
	y int
}

p := &Point{10, 10}
// References have the same syntax for accessing fields
println(p.x)
```

The type of `p` is `&Point`. It's a [reference](#references) to `Point`.
References are similar to Go pointers and C++ references.

### Embedded structs

V doesn't allow subclassing, but it supports embedded structs:

```v
struct Widget {
mut:
	x int
	y int
}

struct Button {
	Widget
	title string
}

mut button := Button{
	title: 'Click me'
}
button.x = 3
```
Without embedding we'd have to name the `Widget` field and do:

```v oksyntax
button.widget.x = 3
```

### Default field values

```v
struct Foo {
	n   int // n is 0 by default
	s   string // s is '' by default
	a   []int // a is `[]int{}` by default
	pos int = -1 // custom default value
}
```

All struct fields are zeroed by default during the creation of the struct.
Array and map fields are allocated.

It's also possible to define custom default values.

### Required fields

```v
struct Foo {
	n int [required]
}
```

You can mark a struct field with the `[required]` attribute, to tell V that
that field must be initialized when creating an instance of that struct.

This example will not compile, since the field `n` isn't explicitly initialized:
```v failcompile
_ = Foo{}
```

<a id='short-struct-initialization-syntax' />

### Short struct literal syntax

```v
struct Point {
	x int
	y int
}

mut p := Point{
	x: 10
	y: 20
}
// you can omit the struct name when it's already known
p = {
	x: 30
	y: 4
}
assert p.y == 4
```

Omitting the struct name also works for returning a struct literal or passing one
as a function argument.

#### Trailing struct literal arguments

V doesn't have default function arguments or named arguments, for that trailing struct
literal syntax can be used instead:

```v
struct ButtonConfig {
	text        string
	is_disabled bool
	width       int = 70
	height      int = 20
}

struct Button {
	text   string
	width  int
	height int
}

fn new_button(c ButtonConfig) &Button {
	return &Button{
		width: c.width
		height: c.height
		text: c.text
	}
}

button := new_button(text: 'Click me', width: 100)
// the height is unset, so it's the default value
assert button.height == 20
```

As you can see, both the struct name and braces can be omitted, instead of:

```v ignore
new_button(ButtonConfig{text:'Click me', width:100})
```

This only works for functions that take a struct for the last argument.

### Access modifiers

Struct fields are private and immutable by default (making structs immutable as well).
Their access modifiers can be changed with
`pub` and `mut`. In total, there are 5 possible options:

```v nofmt
struct Foo {
    a int   // private immutable (default)
mut:
    b int   // private mutable
    c int   // (you can list multiple fields with the same access modifier)
pub:
    d int   // public immutable (readonly)
pub mut:
    e int   // public, but mutable only in parent module
__global:
    f int   // public and mutable both inside and outside parent module
}           // (not recommended to use, that's why the 'global' keyword
            // starts with __)
```

For example, here's the `string` type defined in the `builtin` module:

```v ignore
struct string {
    str byteptr
pub:
    len int
}
```

It's easy to see from this definition that `string` is an immutable type.
The byte pointer with the string data is not accessible outside `builtin` at all.
The `len` field is public, but immutable:
```v failcompile
fn main() {
    str := 'hello'
    len := str.len // OK
    str.len++      // Compilation error
}
```

This means that defining public readonly fields is very easy in V,
no need in getters/setters or properties.

### Methods

```v nofmt
struct User {
    age int
}

fn (u User) can_register() bool {
    return u.age > 16
}

user := User{age: 10}
println(user.can_register()) // "false"

user2 := User{age: 20}
println(user2.can_register()) // "true"
```

V doesn't have classes, but you can define methods on types.
A method is a function with a special receiver argument.
The receiver appears in its own argument list between the `fn` keyword and the method name.

In this example, the `can_register` method has a receiver of type `User` named `u`.
The convention is not to use receiver names like `self` or `this`,
but a short, preferably one letter long, name.

## Functions 2

### Pure functions by default

V functions are pure by default, meaning that their return values are a function of their
arguments only, and their evaluation has no side effects (besides I/O).

This is achieved by a lack of global variables and all function arguments being
immutable by default, even when [references](#references) are passed.

V is not a purely functional language however.

There is a compiler flag to enable global variables (`--enable-globals`), but this is
intended for low-level applications like kernels and drivers.

### Mutable arguments

It is possible to modify function arguments by using the keyword `mut`:

```v nofmt
struct User {
	name string
mut:
    is_registered bool
}

fn (mut u User) register() {
    u.is_registered = true
}

mut user := User{}
println(user.is_registered) // "false"
user.register()
println(user.is_registered) // "true"
```

In this example, the receiver (which is simply the first argument) is marked as mutable,
so `register()` can change the user object. The same works with non-receiver arguments:

```v
fn multiply_by_2(mut arr []int) {
	for i in 0 .. arr.len {
		arr[i] *= 2
	}
}

mut nums := [1, 2, 3]
multiply_by_2(mut nums)
println(nums)
// "[2, 4, 6]"
```

Note, that you have to add `mut` before `nums` when calling this function. This makes
it clear that the function being called will modify the value.

It is preferable to return values instead of modifying arguments.
Modifying arguments should only be done in performance-critical parts of your application
to reduce allocations and copying.

For this reason V doesn't allow the modification of arguments with primitive types (e.g. integers).
Only more complex types such as arrays and maps may be modified.

Use `user.register()` or `user = register(user)`
instead of `register(mut user)`.

V makes it easy to return a modified version of an object:

```v
struct User {
	name          string
	age           int
	is_registered bool
}

fn register(u User) User {
	return {
		u |
		is_registered: true
	}
}

mut user := User{
	name: 'abc'
	age: 23
}
user = register(user)
println(user)
```

### Anonymous & high order functions

```v
fn sqr(n int) int {
	return n * n
}

fn run(value int, op fn (int) int) int {
	return op(value)
}

fn main() {
	println(run(5, sqr)) // "25"
	// Anonymous functions can be declared inside other functions:
	double_fn := fn (n int) int {
		return n + n
	}
	println(run(5, double_fn)) // "10"
	// Functions can be passed around without assigning them to variables:
	res := run(5, fn (n int) int {
		return n + n
	})
}
```

## References

```v
struct Foo {}

fn (foo Foo) bar_method() {
	// ...
}

fn bar_function(foo Foo) {
	// ...
}
```

If a function argument is immutable (like `foo` in the examples above)
V can pass it either by value or by reference. The compiler will decide,
and the developer doesn't need to think about it.

You no longer need to remember whether you should pass the struct by value
or by reference.

You can ensure that the struct is always passed by reference by
adding `&`:

```v
struct Foo {
	abc int
}

fn (foo &Foo) bar() {
	println(foo.abc)
}
```

`foo` is still immutable and can't be changed. For that,
`(mut foo Foo)` must be used.

In general, V's references are similar to Go pointers and C++ references.
For example, a generic tree structure definition would look like this:

```v wip
struct Node<T> {
    val   T
    left  &Node
    right &Node
}
```

## Constants

```v
const (
	pi    = 3.14
	world = '世界'
)

println(pi)
println(world)
```

Constants are declared with `const`. They can only be defined
at the module level (outside of functions).

Constant values can never be changed.

V constants are more flexible than in most languages. You can assign more complex values:

```v
struct Color {
	r int
	g int
	b int
}

fn rgb(r int, g int, b int) Color {
	return Color{
		r: r
		g: g
		b: b
	}
}

const (
	numbers = [1, 2, 3]
	red     = Color{
		r: 255
		g: 0
		b: 0
	}
	// evaluate function call at compile-time
	blue    = rgb(0, 0, 255)
)

println(numbers)
println(red)
println(blue)
```

Global variables are not allowed, so this can be really useful.

<!--
When naming constants, snake_case must be used.
Many people prefer all caps consts: `TOP_CITIES`. This wouldn't work
well in V, because consts are a lot more powerful than in other languages.
They can represent complex structures, and this is used quite often since there
are no globals:
-->

```v ignore
println('Top cities: $top_cities.filter(.usa)')
```

## Builtin functions

Some functions are builtin like `println`. Here is the complete list:

```v ignore
fn print(s string) // print anything on sdtout
fn println(s string) // print anything and a newline on sdtout

fn eprint(s string) // same as print(), but use stderr
fn eprintln(s string) // same as println(), but use stderr

fn exit(code int) // terminate the program with a custom error code
fn panic(s string) // print a message and backtraces on stderr, and terminate the program with error code 1
fn print_backtrace() // print backtraces on stderr
```

`println` is a simple yet powerful builtin function, that can print anything:
strings, numbers, arrays, maps, structs.

```v nofmt
struct User{ name string age int }
println(1) // "1"
println('hi') // "hi"
println([1,2,3]) // "[1, 2, 3]"
println(User{name:'Bob', age:20}) // "User{name:'Bob', age:20}"
```

## Custom print of types

If you want to define a custom print value for your type, simply define a
`.str() string` method:

```v
struct Color {
	r int
	g int
	b int
}

pub fn (c Color) str() string {
	return '{$c.r, $c.g, $c.b}'
}

red := Color{
	r: 255
	g: 0
	b: 0
}
println(red)
```

## Modules

Every file in the root of a folder is part of the same module.
Simple programs don't need to specify module name, in which case it defaults to 'main'.

V is a very modular language. Creating reusable modules is encouraged and is
quite easy to do.
To create a new module, create a directory with your module's name containing
.v files with code:

```shell
cd ~/code/modules
mkdir mymodule
vim mymodule/myfile.v
```
```v failcompile
// myfile.v
module mymodule

// To export a function we have to use `pub`
pub fn say_hi() {
    println('hello from mymodule!')
}
```

You can now use `mymodule` in your code:

```v failcompile
import mymodule

fn main() {
    mymodule.say_hi()
}
```

* Module names should be short, under 10 characters.
* Module names must use `snake_case`.
* Circular imports are not allowed.
* You can have as many .v files in a module as you want.
* You can create modules anywhere.
* All modules are compiled statically into a single executable.

### `init` functions

If you want a module to automatically call some setup/initialization code when it is imported,
you can use a module `init` function:

```v
fn init() {
	// your setup code here ...
}
```

The `init` function cannot be public - it will be called automatically. This feature is
particularly useful for initializing a C library.

## Types 2

### Interfaces

```v
struct Dog {
	breed string
}

struct Cat {}

fn (d Dog) speak() string {
	return 'woof'
}

fn (c Cat) speak() string {
	return 'meow'
}

interface Speaker {
	speak() string
}

dog := Dog{'Leonberger'}
cat := Cat{}
mut arr := []Speaker{}
arr << dog
arr << cat
for item in arr {
	item.speak()
}
```

A type implements an interface by implementing its methods.
There is no explicit declaration of intent, no "implements" keyword.

We can test the underlying type of an interface using dynamic cast operators:
```v oksyntax
fn announce(s Speaker) {
	if s is Dog {
		println('a $s.breed') // `s` is automatically cast to `Dog` (smart cast)
	} else if s is Cat {
		println('a cat')
	} else {
		println('something else')
	}
}
```
For more information, see [Dynamic casts](#dynamic-casts).

### Enums

```v nofmt
enum Color {
    red green blue
}

mut color := Color.red
// V knows that `color` is a `Color`. No need to use `color = Color.green` here.
color = .green
println(color) // "green"

match color {
    .red { println('the color was red') }
    .green { println('the color was green') }
    .blue { println('the color was blue') }
}
```

Enum match must be exhaustive or have an `else` branch.
This ensures that if a new enum field is added, it's handled everywhere in the code.

### Sum types

A sum type instance can hold a value of several different types. Use the `type`
keyword to declare a sum type:

```v
struct Moon {}

struct Mars {}

struct Venus {}

type World = Mars | Moon | Venus

sum := World(Moon{})
assert sum.type_name() == 'Moon'
println(sum)
```
The built-in method `type_name` returns the name of the currently held
type.

#### Dynamic casts

To check whether a sum type instance holds a certain type, use `sum is Type`.
To cast a sum type to one of its variants you can use `sum as Type`:

```v
struct Moon {}

struct Mars {}

struct Venus {}

type World = Mars | Moon | Venus

fn (m Mars) dust_storm() bool {
	return true
}

fn main() {
	mut w := World(Moon{})
	assert w is Moon
	w = Mars{}
	// use `as` to access the Mars instance
	mars := w as Mars
	if mars.dust_storm() {
		println('bad weather!')
	}
}
```

`as` will panic if `w` doesn't hold a `Mars` instance.
A safer way is to use a smart cast.

#### Smart casting

```v oksyntax
if w is Mars {
	assert typeof(w).name == 'Mars'
	if w.dust_storm() {
		println('bad weather!')
	}
}
```
`w` has type `Mars` inside the body of the `if` statement. This is
known as *flow-sensitive typing*. You can also specify a variable name:

```v ignore
if w is Mars as mars {
    assert typeof(w).name == 'World'
    if mars.dust_storm() {
        println('bad weather!')
    }
}
```
`w` keeps its original type. This form is necessary if `w` is a more
complex expression than just a variable name.

#### Matching sum types

You can also use `match` to determine the variant:

```v
struct Moon {}

struct Mars {}

struct Venus {}

type World = Mars | Moon | Venus

fn open_parachutes(n int) {
	println(n)
}

fn land(w World) {
	match w {
		Moon {} // no atmosphere
		Mars {
			// light atmosphere
			open_parachutes(3)
		}
		Venus {
			// heavy atmosphere
			open_parachutes(1)
		}
	}
}
```

`match` must have a pattern for each variant or have an `else` branch.

There are two ways to access the cast variant inside a match branch:
- the shadowed match variable
- using `as` to specify a variable name

```v ignore
struct Moon {}
struct Mars {}
struct Venus {}

type World = Moon | Mars | Venus

fn (m Moon) moon_walk() {}
fn (m Mars) shiver() {}
fn (v Venus) sweat() {}

fn pass_time(w World) {
    match w {
        // using the shadowed match variable, in this case `w` (smart cast)
        Moon { w.moon_walk() }
        Mars { w.shiver() }
        else {}
    }
    // using `as` to specify a name for each value
    match w as var {
        Mars  { var.shiver() }
        Venus { var.sweat() }
        else {
            // w is of type World
            assert w is Moon
        }
    }
}
```

Note: shadowing only works when the match expression is a variable.
It will not work on struct fields, array indexes, or map keys.

### Option/Result types and error handling

Option types are declared with `?Type`:
```v
struct User {
	id   int
	name string
}

struct Repo {
	users []User
}

fn (r Repo) find_user_by_id(id int) ?User {
	for user in r.users {
		if user.id == id {
			// V automatically wraps this into an option type
			return user
		}
	}
	return error('User $id not found')
}

fn main() {
	repo := Repo{
		users: [User{1, 'Andrew'}, User{2, 'Bob'},
			User{10, 'Charles'},
		]
	}
	user := repo.find_user_by_id(10) or { // Option types must be handled by `or` blocks
		return
	}
	println(user.id) // "10"
	println(user.name) // "Charles"
}
```

V combines `Option` and `Result` into one type, so you don't need to decide which one to use.

The amount of work required to "upgrade" a function to an optional function is minimal;
you have to add a `?` to the return type and return an error when something goes wrong.

If you don't need to return an error message, you can simply `return none`
(this is a more efficient equivalent of `return error("")`).

This is the primary mechanism for error handling in V. They are still values, like in Go,
but the advantage is that errors can't be unhandled, and handling them is a lot less verbose.
Unlike other languages, V does not handle exceptions with `throw/try/catch` blocks.

`err` is defined inside an `or` block and is set to the string message passed
to the `error()` function. `err` is empty if `none` was returned.

```v oksyntax
user := repo.find_user_by_id(7) or {
	println(err) // "User 7 not found"
	return
}
```

### Handling optionals

There are four ways of handling an optional. The first method is to
propagate the error:

```v
import net.http

fn f(url string) ?string {
	resp := http.get(url) ?
	return resp.text
}
```

`http.get` returns `?http.Response`. Because `?` follows the call, the
error will be propagated to the caller of `f`. When using `?` after a
function call producing an optional, the enclosing function must return
an optional as well. If error propagation is used in the `main()`
function it will `panic` instead, since the error cannot be propagated
any further.

The body of `f` is essentially a condensed version of:

```v ignore
    resp := http.get(url) or { return error(err) }
    return resp.text
```

---
The second method is to break from execution early:

```v oksyntax
user := repo.find_user_by_id(7) or { return }
```

Here, you can either call `panic()` or `exit()`, which will stop the execution of the
entire program, or use a control flow statement (`return`, `break`, `continue`, etc)
to break from the current block.
Note that `break` and `continue` can only be used inside a `for` loop.

V does not have a way to forcibly "unwrap" an optional (as other languages do,
for instance Rust's `unwrap()` or Swift's `!`). To do this, use `or { panic(err) }` instead.

---
The third method is to provide a default value at the end of the `or` block.
In case of an error, that value would be assigned instead,
so it must have the same type as the content of the `Option` being handled.

```v nofmt
fn do_something(s string) ?string {
	if s == 'foo' {
		return 'foo'
	}
	return error('invalid string') // Could be `return none` as well
}

a := do_something('foo') or { 'default' } // a will be 'foo'
b := do_something('bar') or { 'default' } // b will be 'default'
println(a)
println(b)
```

---
The fourth method is to use `if` unwrapping:

```v
import net.http

if resp := http.get('https://google.com') {
	println(resp.text) // resp is a http.Response, not an optional
} else {
	println(err)
}
```
Above, `http.get` returns a `?http.Response`. `resp` is only in scope for the first
`if` branch. `err` is only in scope for the `else` branch.

## Generics

```v wip

struct Repo<T> {
    db DB
}

fn new_repo<T>(db DB) Repo<T> {
    return Repo<T>{db: db}
}

// This is a generic function. V will generate it for every type it's used with.
fn (r Repo<T>) find_by_id(id int) ?T {
    table_name := T.name // in this example getting the name of the type gives us the table name
    return r.db.query_one<T>('select * from $table_name where id = ?', id)
}

db := new_db()
users_repo := new_repo<User>(db) // returns Repo<User>
posts_repo := new_repo<Post>(db) // returns Repo<Post>
user := users_repo.find_by_id(1)? // find_by_id<User>
post := posts_repo.find_by_id(1)? // find_by_id<Post>
```
At the moment only one type parameter named `T` is supported.

Currently generic function definitions must declare their type parameters, but in
future V will infer generic type parameters from single-letter type names in
runtime parameter types. This is why `find_by_id` can omit `<T>`, because the
receiver argument `r` uses a generic type `T`.

Another example:
```v nofmt
fn compare<T>(a T, b T) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// compare<int>
println(compare(1, 0)) // Outputs: 1
println(compare(1, 1)) //          0
println(compare(1, 2)) //         -1

// compare<string>
println(compare('1', '0')) // Outputs: 1
println(compare('1', '1')) //          0
println(compare('1', '2')) //         -1

// compare<f64>
println(compare(1.1, 1.0)) // Outputs: 1
println(compare(1.1, 1.1)) //          0
println(compare(1.1, 1.2)) //         -1
```


## Concurrency

V's model of concurrency is very similar to Go's. To run `foo()` concurrently, just
call it with `go foo()`. Right now, it launches the function on a new system
thread. Soon coroutines and a scheduler will be implemented.

```v
import sync
import time

fn task(id int, duration int, mut wg sync.WaitGroup) {
	println('task $id begin')
	time.sleep_ms(duration)
	println('task $id end')
	wg.done()
}

fn main() {
	mut wg := sync.new_waitgroup()
	wg.add(3)
	go task(1, 500, mut wg)
	go task(2, 900, mut wg)
	go task(3, 100, mut wg)
	wg.wait()
	println('done')
}

// Output:
// task 1 begin
// task 2 begin
// task 3 begin
// task 3 end
// task 1 end
// task 2 end
// done
```

### Channels
Channels are the preferred way to communicate between coroutines. V's channels work basically like
those in Go. You can push objects into a channel on one end and pop objects from the other end.
Channels can be buffered or unbuffered and it is possible to `select` from multiple channels.

#### Syntax and Usage
Channels have the type `chan objtype`. An optional buffer length can specified as the `cap` property
in the declaration:

```v nofmt
ch := chan int{}          // unbuffered - "synchronous"
ch2 := chan f64{cap: 100} // buffer length 100
```

Channels do not have to be declared as `mut`. The buffer length is not part of the type but
a property of the individual channel object. Channels can be passed to coroutines like normal
variables:

```v
import sync

fn f(ch chan int) {
	// ...
}

fn main() {
	ch := chan int{}
	go f(ch)
	// ...
}
```

Objects can be pushed to channels using the arrow operator. The same operator can be used to
pop objects from the other end:

```v nofmt
import sync

mut ch := chan int{}
mut ch2 := chan f64{}
n := 5
x := 7.3
ch <- n    // push
ch2 <- x
mut y := f64(0.0)
m := <-ch  // pop creating new variable
y = <-ch2  // pop into existing variable
```

A channel can be closed to indicate that no further objects can be pushed. Any attempt
to do so will then result in a runtime panic (with the exception of `select` and
`try_push()` - see below). Attempts to pop will return immediately if the
associated channel has been closed and the buffer is empty. This situation can be
handled using an or branch (see [Handling Optionals](#handling-optionals)).

```v wip
mut ch := chan int{}
mut ch2 := chan f64{}
// ...
ch.close()
// ...
m := <-ch or {
    println('channel has been closed')
}

// propagate error
y := <-ch2 ?
```

#### Channel Select

The `select` command allows monitoring several channels at the same time
without noticeable CPU load.  It consists of a list of possible transfers and associated branches
of statements - similar to the [match](#match) command:
```v wip
import time
fn main () {
  mut c := chan f64{}
  mut ch := chan f64{}
  mut ch2 := chan f64{}
  mut ch3 := chan f64{}
  mut b := 0.0
  // ...
  select {
    a := <-ch {
        // do something with `a`
    }
    b = <-ch2 {
        // do something with predeclared variable `b`
    }
    ch3 <- c {
        // do something if `c` was sent
    }
    > 500 * time.millisecond {
        // do something if no channel has become ready within 0.5s
    }
  }
}
```

The timeout branch is optional. If it is absent `select` waits for an unlimited amount of time.
It is also possible to proceed immediately if no channel is ready in the moment `select` is called
by adding an `else { ... }` branch. `else` and `> timeout` are mutually exclusive.

The `select` command can be used as an *expression* of type `bool`
that becomes `false` if all channels are closed:
```v wip
if select {
    ch <- a {
        // ...
    }
} {
    // channel was open
} else {
    // channel is closed
}
```

#### Special Channel Features

For special purposes there are some builtin properties and methods:
```v nofmt
import sync

struct Abc{x int}

a := 2.13
mut ch := chan f64{}

res := ch.try_push(a)      // try to perform `ch <- a`
println(res)
l := ch.len                // number of elements in queue
c := ch.cap                // maximum queue length
println(l)
println(c)

// mut b := Abc{}
// mut ch2 := chan f64{}
// res2 := ch2.try_pop(mut b) // try to perform `b = <-ch2
```

The `try_push/pop()` methods will return immediately with one of the results
`.success`, `.not_ready` or `.closed` - dependent on whether the object has been transferred or
the reason why not.
Usage of these methods and properties in production is not recommended -
algorithms based on them are often subject to race conditions. Use `select` instead.

Data can be exchanged between a coroutine and the calling thread via a shared variable.
Such variables should be created as references and passed to the coroutine as `mut`.
The underlying `struct` should also contain a `mutex` to lock concurrent access:

```v
import sync

struct St {
mut:
	x   int // share data
	mtx &sync.Mutex
}

fn (mut b St) g() {
	b.mtx.m_lock()
	// read/modify/write b.x
	b.mtx.unlock()
}

fn caller() {
	mut a := &St{ // create as reference so it's on the heap
		x: 10
		mtx: sync.new_mutex()
	}
	go a.g()
	a.mtx.m_lock()
	// read/modify/write a.x
	a.mtx.unlock()
}
```

## Decoding JSON

```v
import json

struct Foo {
	x int
}

struct User {
	name      string
	age       int
	// Use the `skip` attribute to skip certain fields
	foo       Foo    [skip]
	// If the field name is different in JSON, it can be specified
	last_name string [json: lastName]
}

data := '{ "name": "Frodo", "lastName": "Baggins", "age": 25 }'
user := json.decode(User, data) or {
	eprintln('Failed to decode json')
	return
}
println(user.name)
println(user.last_name)
println(user.age)
// You can also decode JSON arrays:
sfoos := '[{"x":123},{"x":456}]'
foos := json.decode([]Foo, sfoos) ?
println(foos[0].x)
println(foos[1].x)
```

Because of the ubiquitous nature of JSON, support for it is built directly into V.

The `json.decode` function takes two arguments:
the first is the type into which the JSON value should be decoded and
the second is a string containing the JSON data.

V generates code for JSON encoding and decoding.
No runtime reflection is used. This results in much better performance.

## Testing

### Asserts

```v
fn foo(mut v []int) {
	v[0] = 1
}

mut v := [20]
foo(mut v)
assert v[0] < 4
```
An `assert` statement checks that its expression evaluates to `true`. If an assert fails,
the program will abort. Asserts should only be used to detect programming errors. When an
assert fails it is reported to *stderr*, and the values on each side of a comparison operator
(such as `<`, `==`) will be printed when possible. This is useful to easily find an
unexpected value. Assert statements can be used in any function.

### Test files

```v
// hello.v
module main

fn hello() string {
	return 'Hello world'
}

fn main() {
	println(hello())
}
```

```v failcompile
module main
// hello_test.v
fn test_hello() {
    assert hello() == 'Hello world'
}
```
To run the test above, use `v hello_test.v`. This will check that the function `hello` is
producing the correct output. V executes all test functions in the file.

* All test functions have to be inside a test file whose name ends in `_test.v`.
* Test function names must begin with `test_` to mark them for execution.
* Normal functions can also be defined in test files, and should be called manually. Other
  symbols can also be defined in test files e.g. types.
* There are two kinds of tests: external and internal.
* Internal tests must *declare* their module, just like all other .v
files from the same module. Internal tests can even call private functions in
the same module.
* External tests must *import* the modules which they test. They do not
have access to the private functions/types of the modules. They can test only
the external/public API that a module provides.

In the example above, `test_hello` is an internal test, that can call
the private function `hello()` because `hello_test.v` has `module main`,
just like `hello.v`, i.e. both are part of the same module. Note also that
since `module main` is a regular module like the others, internal tests can
be used to test private functions in your main program .v files too.

You can also define special test functions in a test file:
* `testsuite_begin` which will be run *before* all other test functions.
* `testsuite_end` which will be run *after* all other test functions.

#### Running tests

To run test functions in an individual test file, use `v foo_test.v`.

To test an entire module, use `v test mymodule`. You can also use `v test .` to test
everything inside your current folder (and subfolders). You can pass the `-stats`
option to see more details about the individual tests run.

## Memory management

V avoids doing unnecessary allocations in the first place by using value types,
string buffers, promoting a simple abstraction-free code style.

Most objects (~90-100%) are freed by V's autofree engine: the compiler inserts
necessary free calls automatically during compilation. Remaining small percentage
of objects is freed via reference counting.

The developer doesn't need to change anything in their code. "It just works", like in
Python, Go, or Java, except there's no heavy GC tracing everything or expensive RC for
each object.

For developers willing to have more low level control, autofree can be disabled with
`-noautofree`.

Note: right now autofree is hidden behind the -autofree flag. It will be enabled by
default in V 0.3.

For example:

```v
import strings

fn draw_text(s string, x int, y int) {
	// ...
}

fn draw_scene() {
	// ...
	name1 := 'abc'
	name2 := 'def ghi'
	draw_text('hello $name1', 10, 10)
	draw_text('hello $name2', 100, 10)
	draw_text(strings.repeat(`X`, 10000), 10, 50)
	// ...
}
```

The strings don't escape `draw_text`, so they are cleaned up when
the function exits.

In fact, the first two calls won't result in any allocations at all.
These two strings are small,
V will use a preallocated buffer for them.

```v
struct User {
	name string
}

fn test() []int {
	number := 7 // stack variable
	user := User{} // struct allocated on stack
	numbers := [1, 2, 3] // array allocated on heap, will be freed as the function exits
	println(number)
	println(user)
	println(numbers)
	numbers2 := [4, 5, 6] // array that's being returned, won't be freed here
	return numbers2
}
```

## ORM

(This is still in an alpha state)

V has a built-in ORM (object-relational mapping) which supports SQLite,
and will soon support MySQL, Postgres, MS SQL, and Oracle.

V's ORM provides a number of benefits:

- One syntax for all SQL dialects. (Migrating between databases becomes much easier.)
- Queries are constructed using V's syntax. (There's no need to learn another syntax.)
- Safety. (All queries are automatically sanitised to prevent SQL injection.)
- Compile time checks. (This prevents typos which can only be caught during runtime.)
- Readability and simplicity. (You don't need to manually parse the results of a query and
    then manually construct objects from the parsed results.)

```v nofmt
import sqlite
struct Customer { // struct name has to be the same as the table name (for now)
    id int // a field named `id` of integer type must be the first field
    name string
    nr_orders int
    country string
}

db := sqlite.connect('customers.db')?

// select count(*) from Customer
nr_customers := sql db { select count from Customer }
println('number of all customers: $nr_customers')

// V syntax can be used to build queries
// db.select returns an array
uk_customers := sql db { select from Customer where country == 'uk' && nr_orders > 0 }
println(uk_customers.len)
for customer in uk_customers {
    println('$customer.id - $customer.name')
}

// by adding `limit 1` we tell V that there will be only one object
customer := sql db { select from Customer where id == 1 limit 1 }
println('$customer.id - $customer.name')

// insert a new customer
new_customer := Customer{name: 'Bob', nr_orders: 10}
sql db { insert new_customer into Customer }
```

For more examples, see <a href='https://github.com/vlang/v/blob/master/vlib/orm/orm_test.v'>vlib/orm/orm_test.v</a>.

## Writing Documentation

The way it works is very similar to Go. It's very simple: there's no need to
write documentation separately for your code,
vdoc will generate it from docstrings in the source code.

Documentation for each function/type/const must be placed right before the declaration:

```v
// clearall clears all bits in the array
fn clearall() {
}
```

The comment must start with the name of the definition.

Sometimes one line isn't enough to explain what a function does, in that case comments should
span to the documented function using single line comments:

```v
// copy_all recursively copies all elements of the array by their value,
// if `dupes` is false all duplicate values are eliminated in the process.
fn copy_all(dupes bool) {
	// ...
}
```

By convention it is preferred that comments are written in *present tense*.

An overview of the module must be placed in the first comment right after the module's name.

To generate documentation use vdoc, for example `v doc net.http`.

## Tools

### v fmt

You don't need to worry about formatting your code or setting style guidelines.
`v fmt` takes care of that:

```shell
v fmt file.v
```

It's recommended to set up your editor, so that `v fmt -w` runs on every save.
A vfmt run is usually pretty cheap (takes <30ms).

Always run `v fmt -w file.v` before pushing your code.

### Profiling

V has good support for profiling your programs: `v -profile profile.txt run file.v`
That will produce a profile.txt file, which you can then analyze.

The generated profile.txt file will have lines with 4 columns:
a) how many times a function was called
b) how much time in total a function took (in ms)
c) how much time on average, a call to a function took (in ns)
d) the name of the v function

You can sort on column 3 (average time per function) using:
`sort -n -k3 profile.txt|tail`

You can also use stopwatches to measure just portions of your code explicitly:
```v
import time

fn main() {
	sw := time.new_stopwatch({})
	println('Hello world')
	println('Greeting the world took: ${sw.elapsed().nanoseconds()}ns')
}
```

# Advanced Topics

## Memory-unsafe code

Sometimes for efficiency you may want to write low-level code that can potentially
corrupt memory or be vulnerable to security exploits. V supports writing such code,
but not by default.

V requires that any potentially memory-unsafe operations are marked intentionally.
Marking them also indicates to anyone reading the code that there could be
memory-safety violations if there was a mistake.

Examples of potentially memory-unsafe operations are:

* Pointer arithmetic
* Pointer indexing
* Conversion to pointer from an incompatible type
* Calling certain C functions, e.g. `free`, `strlen` and `strncmp`.

To mark potentially memory-unsafe operations, enclose them in an `unsafe` block:

```v wip
// allocate 2 uninitialized bytes & return a reference to them
mut p := unsafe { malloc(2) }
p[0] = `h` // Error: pointer indexing is only allowed in `unsafe` blocks
unsafe {
    p[0] = `h` // OK
    p[1] = `i`
}
p++ // Error: pointer arithmetic is only allowed in `unsafe` blocks
unsafe {
    p++ // OK
}
assert *p == `i`
```

Best practice is to avoid putting memory-safe expressions inside an `unsafe` block,
so that the reason for using `unsafe` is as clear as possible. Generally any code
you think is memory-safe should not be inside an `unsafe` block, so the compiler
can verify it.

If you suspect your program does violate memory-safety, you have a head start on
finding the cause: look at the `unsafe` blocks (and how they interact with
surrounding code).

* Note: This is work in progress.

## Calling C functions from V

```v
#flag -lsqlite3
#include "sqlite3.h"
// See also the example from https://www.sqlite.org/quickstart.html
struct C.sqlite3 {
}

struct C.sqlite3_stmt {
}

type FnSqlite3Callback = fn (voidptr, int, &charptr, &charptr) int

fn C.sqlite3_open(charptr, &&C.sqlite3) int

fn C.sqlite3_close(&C.sqlite3) int

fn C.sqlite3_column_int(stmt &C.sqlite3_stmt, n int) int

// ... you can also just define the type of parameter and leave out the C. prefix
fn C.sqlite3_prepare_v2(&sqlite3, charptr, int, &&sqlite3_stmt, &charptr) int

fn C.sqlite3_step(&sqlite3_stmt)

fn C.sqlite3_finalize(&sqlite3_stmt)

fn C.sqlite3_exec(db &sqlite3, sql charptr, FnSqlite3Callback voidptr, cb_arg voidptr, emsg &charptr) int

fn C.sqlite3_free(voidptr)

fn my_callback(arg voidptr, howmany int, cvalues &charptr, cnames &charptr) int {
	unsafe {
		for i in 0 .. howmany {
			print('| ${cstring_to_vstring(cnames[i])}: ${cstring_to_vstring(cvalues[i]):20} ')
		}
	}
	println('|')
	return 0
}

fn main() {
	db := &C.sqlite3(0) // this means `sqlite3* db = 0`
	// passing a string literal to a C function call results in a C string, not a V string
	C.sqlite3_open('users.db', &db)
	// C.sqlite3_open(db_path.str, &db)
	query := 'select count(*) from users'
	stmt := &C.sqlite3_stmt(0)
	// NB: you can also use the `.str` field of a V string,
	// to get its C style zero terminated representation
	C.sqlite3_prepare_v2(db, query.str, -1, &stmt, 0)
	C.sqlite3_step(stmt)
	nr_users := C.sqlite3_column_int(stmt, 0)
	C.sqlite3_finalize(stmt)
	println('There are $nr_users users in the database.')
	//
	error_msg := charptr(0)
	query_all_users := 'select * from users'
	rc := C.sqlite3_exec(db, query_all_users.str, my_callback, 7, &error_msg)
	if rc != C.SQLITE_OK {
		eprintln(cstring_to_vstring(error_msg))
		C.sqlite3_free(error_msg)
	}
	C.sqlite3_close(db)
}
```

### #flag

Add `#flag` directives to the top of your V files to provide C compilation flags like:

- `-I` for adding C include files search paths
- `-l` for adding C library names that you want to get linked
- `-L` for adding C library files search paths
- `-D` for setting compile time variables

You can use different flags for different targets.
Currently the `linux`, `darwin` , `freebsd`, and `windows` flags are supported.

NB: Each flag must go on its own line (for now)

```v oksyntax
#flag linux -lsdl2
#flag linux -Ivig
#flag linux -DCIMGUI_DEFINE_ENUMS_AND_STRUCTS=1
#flag linux -DIMGUI_DISABLE_OBSOLETE_FUNCTIONS=1
#flag linux -DIMGUI_IMPL_API=
```

### #pkgconfig

Add `#pkgconfig` directive is used to tell the compiler which modules should be used for compiling
and linking using the pkg-config files provided by the respective dependencies.

As long as backticks can't be used in `#flag` and spawning processes is not desirable for security
and portability reasons, V uses its own pkgconfig library that is compatible with the standard
freedesktop one.

If no flags are passed it will add `--cflags` and `--libs`, both lines below do the same:

```v oksyntax
#pkgconfig r_core
#pkgconfig --cflags --libs r_core
```

The `.pc` files are looked up into a hardcoded list of default pkg-config paths, the user can add
extra paths by using the `PKG_CONFIG_PATH` environment variable. Multiple modules can be passed.

### Including C code

You can also include C code directly in your V module.
For example, let's say that your C code is located in a folder named 'c' inside your module folder.
Then:

* Put a v.mod file inside the toplevel folder of your module (if you
created your module with `v new` you already have v.mod file). For
example:
```v ignore
Module {
	name: 'mymodule',
	description: 'My nice module wraps a simple C library.',
	version: '0.0.1'
	dependencies: []
}
```


* Add these lines to the top of your module:
```v oksyntax
#flag -I @VROOT/c
#flag @VROOT/c/implementation.o
#include "header.h"
```
NB: @VROOT will be replaced by V with the *nearest parent folder, where there is a v.mod file*.
Any .v file beside or below the folder where the v.mod file is,
can use `#flag @VROOT/abc` to refer to this folder.
The @VROOT folder is also *prepended* to the module lookup path,
so you can *import* other modules under your @VROOT, by just naming them.

The instructions above will make V look for an compiled .o file in
your module `folder/c/implementation.o`.
If V finds it, the .o file will get linked to the main executable, that used the module.
If it does not find it, V assumes that there is a `@VROOT/c/implementation.c` file,
and tries to compile it to a .o file, then will use that.

This allows you to have C code, that is contained in a V module, so that its distribution is easier.
You can see a complete minimal example for using C code in a V wrapper module here:
[project_with_c_code](https://github.com/vlang/v/tree/master/vlib/v/tests/project_with_c_code).
Another example, demonstrating passing structs from C to V and back again:
[interoperate between C to V to C](https://github.com/vlang/v/tree/master/vlib/v/tests/project_with_c_code_2).

You can use `-cflags` to pass custom flags to the backend C compiler.
You can also use `-cc` to change the default C backend compiler.
For example: `-cc gcc-9 -cflags -fsanitize=thread`.

### C types

Ordinary zero terminated C strings can be converted to V strings with
`unsafe { charptr(cstring).vstring() }` or if you know their length already with
`unsafe { charptr(cstring).vstring_with_len(len) }`.

NB: The .vstring() and .vstring_with_len() methods do NOT create a copy of the `cstring`,
so you should NOT free it after calling the method `.vstring()`.
If you need to make a copy of the C string (some libc APIs like `getenv` pretty much require that,
since they return pointers to internal libc memory), you can use `cstring_to_vstring(cstring)`.

On Windows, C APIs often return so called `wide` strings (utf16 encoding).
These can be converted to V strings with `string_from_wide(&u16(cwidestring))` .

V has these types for easier interoperability with C:

- `voidptr` for C's `void*`,
- `byteptr` for C's `byte*` and
- `charptr` for C's `char*`.
- `&charptr` for C's `char**`

To cast a `voidptr` to a V reference, use `user := &User(user_void_ptr)`.

`voidptr` can also be dereferenced into a V struct through casting: `user := User(user_void_ptr)`.

[socket.v has an example which calls C code from V](https://github.com/vlang/v/blob/master/vlib/net/socket.v) .

## Debugging generated C code

To debug issues in the generated C code, you can pass these flags:

- `-cg` - produces a less optimized executable with more debug information in it.
- `-showcc` - prints the C command that is used to build the program.

For the best debugging experience, you can pass all of them at the same time:
`v -cg -showcc yourprogram.v`,
then just run your debugger (gdb/lldb) or IDE on the produced executable `yourprogram`.

If you just want to inspect the generated C code,
without further compilation, you can also use the `-o` flag (e.g. `-o file.c`).
This will make V produce the `file.c` then stop.

If you want to see the generated C source code for *just* a single C function,
for example `main`, you can use: `-printfn main -o file.c`.

To see a detailed list of all flags that V supports,
use `v help`, `v help build` and `v help build-c`.

## Conditional compilation

### Compile time if

```v
// Support for multiple conditions in one branch
$if ios || android {
	println('Running on a mobile device!')
}
$if linux && x64 {
	println('64-bit Linux.')
}
// Usage as expression
os := $if windows { 'Windows' } $else { 'UNIX' }
println('Using $os')
// $else-$if branches
$if tinyc {
	println('tinyc')
} $else $if clang {
	println('clang')
} $else $if gcc {
	println('gcc')
} $else {
	println('different compiler')
}
$if test {
	println('testing')
}
// v -cg ...
$if debug {
	println('debugging')
}
// v -d option ...
$if option ? {
	println('custom option')
}
```

If you want an `if` to be evaluated at compile time it must be prefixed with a `$` sign.
Right now it can be used to detect an OS, compiler, platform or compilation options.
`$if debug` is a special option like `$if windows` or `$if x32`.
If you're using a custom ifdef, then you do need `$if option ? {}` and compile with`v -d option`.
Full list of builtin options:
| OS                            | Compilers         | Platforms             | Other                 |
| ---                           | ---               | ---                   | ---                   |
| `windows`, `linux`, `macos`   | `gcc`, `tinyc`    | `amd64`, `aarch64`    | `debug`, `test`, `js` |
| `mac`, `darwin`, `ios`,       | `clang`, `mingw`  | `x64`, `x32`          | `glibc`, `prealloc`   |
| `android`,`mach`, `dragonfly` | `msvc`            | `little_endian`       | `no_bounds_checking`  |
| `gnu`, `hpux`, `haiku`, `qnx` | `cplusplus`       | `big_endian`          | |
| `solaris`, `linux_or_macos`   | | | |

### Environment specific files

If a file has an environment-specific suffix, it will only be compiled for that environment.

- `.js.v` => will be used only by the JS backend. These files can contain JS. code.
- `.c.v` => will be used only by the C backend. These files can contain C. code.
- `.x64.v` => will be used only by V's x64 backend.
- `_nix.c.v` => will be used only on Unix systems (non Windows).
- `_${os}.c.v` => will be used only on the specific `os` system.
For example, `_windows.c.v` will be used only when compiling on Windows, or with `-os windows`.
- `_default.c.v` => will be used only if there is NOT a more specific platform file.
For example, if you have both `file_linux.c.v` and `file_default.c.v`,
and you are compiling for linux, then only `file_linux.c.v` will be used,
and `file_default.c.v` will be ignored.

Here is a more complete example:
main.v:
```v ignore
module main
fn main() { println(message) }
```

main_default.c.v:
```v ignore
module main
const ( message = 'Hello world' )
```

main_linux.c.v:
```v ignore
module main
const ( message = 'Hello linux' )
```

main_windows.c.v:
```v ignore
module main
const ( message = 'Hello windows' )
```

With the example above:
- when you compile for windows, you will get 'Hello windows'
- when you compile for linux, you will get 'Hello linux'
- when you compile for any other platform, you will get the
non specific 'Hello world' message.

## Compile time pseudo variables

V also gives your code access to a set of pseudo string variables,
that are substituted at compile time:

- `@FN` => replaced with the name of the current V function
- `@MOD` => replaced with the name of the current V module
- `@STRUCT` => replaced with the name of the current V struct
- `@FILE` => replaced with the path of the V source file
- `@LINE` => replaced with the V line number where it appears (as a string).
- `@COLUMN` => replaced with the column where it appears (as a string).
- `@VEXE` => replaced with the path to the V compiler
- `@VHASH`  => replaced with the shortened commit hash of the V compiler (as a string).
- `@VMOD_FILE` => replaced with the contents of the nearest v.mod file (as a string).

That allows you to do the following example, useful while debugging/logging/tracing your code:
```v
eprintln('file: ' + @FILE + ' | line: ' + @LINE + ' | fn: ' + @MOD + '.' + @FN)
```

Another example, is if you want to embed the version/name from v.mod *inside* your executable:
```v ignore
import v.vmod
vm := vmod.decode( @VMOD_FILE ) or { panic(err) }
eprintln('$vm.name $vm.version\n $vm.description')
```

## Performance tuning

The generated C code is usually fast enough, when you compile your code
with `-prod`. There are some situations though, where you may want to give
additional hints to the compiler, so that it can further optimize some
blocks of code.

NB: These are *rarely* needed, and should not be used, unless you
*profile your code*, and then see that there are significant benefits for them.
To cite gcc's documentation: "programmers are notoriously bad at predicting
how their programs actually perform".

`[inline]` - you can tag functions with `[inline]`, so the C compiler will
try to inline them, which in some cases, may be beneficial for performance,
but may impact the size of your executable.

`[direct_array_access]` - in functions tagged with `[direct_array_access]`
the compiler will translate array operations directly into C array operations -
omiting bounds checking. This may save a lot of time in a function that iterates
over an array but at the cost of making the function unsafe - unless
the boundaries will be checked by the user.

`if _likely_(bool expression) {` this hints the C compiler, that the passed
boolean expression is very likely to be true, so it can generate assembly
code, with less chance of branch misprediction. In the JS backend,
that does nothing.

`if _unlikely_(bool expression) {` similar to `_likely_(x)`, but it hints that
the boolean expression is highly improbable. In the JS backend, that does nothing.

<a id='Reflection via codegen'>

## Compile-time reflection

Having built-in JSON support is nice, but V also allows you to create efficient
serializers for any data format. V has compile-time `if` and `for` constructs:

```v wip
// TODO: not fully implemented

struct User {
    name string
    age  int
}

// Note: T should be passed a struct name only
fn decode<T>(data string) T {
    mut result := T{}
    // compile-time `for` loop
    // T.fields gives an array of a field metadata type
    $for field in T.fields {
        $if field.typ is string {
            // $(string_expr) produces an identifier
            result.$(field.name) = get_string(data, field.name)
        } $else $if field.typ is int {
            result.$(field.name) = get_int(data, field.name)
        }
    }
    return result
}

// `decode<User>` generates:
fn decode_User(data string) User {
    mut result := User{}
    result.name = get_string(data, 'name')
    result.age = get_int(data, 'age')
    return result
}
```

## Limited operator overloading

```v
struct Vec {
	x int
	y int
}

fn (a Vec) str() string {
	return '{$a.x, $a.y}'
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x, a.y + b.y}
}

fn (a Vec) - (b Vec) Vec {
	return Vec{a.x - b.x, a.y - b.y}
}

fn main() {
	a := Vec{2, 3}
	b := Vec{4, 5}
	println(a + b) // "{6, 8}"
	println(a - b) // "{-2, -2}"
}
```

Operator overloading goes against V's philosophy of simplicity and predictability.
But since scientific and graphical applications are among V's domains,
operator overloading is an important feature to have in order to improve readability:

`a.add(b).add(c.mul(d))` is a lot less readable than `a + b + c * d`.

To improve safety and maintainability, operator overloading is limited:

- It's only possible to overload `+, -, *, /, %` operators.
- Calling other functions inside operator functions is not allowed.
- Operator functions can't modify their arguments.
- Both arguments must have the same type (just like with all operators in V).

## Inline assembly

TODO: not implemented yet

```v failcompile
fn main() {
    a := 10
    asm x64 {
        mov eax, [a]
        add eax, 10
        mov [a], eax
    }
}
```

## Translating C to V

TODO: translating C to V will be available in V 0.3.

V can translate your C code to human readable V code and generate V wrappers on top of C libraries.


Let's create a simple program `test.c` first:

```c
#include "stdio.h"

int main() {
	for (int i = 0; i < 10; i++) {
		printf("hello world\n");
	}
        return 0;
}
```

Run `v translate test.c`, and V will generate `test.v`:

```v
fn main() {
	for i := 0; i < 10; i++ {
		println('hello world')
	}
}
```

To generate a wrapper on top of a C library use this command:

```bash
v wrapper c_code/libsodium/src/libsodium
```

This will generate a directory `libsodium` with a V module.

Example of a C2V generated libsodium wrapper:

https://github.com/medvednikov/libsodium

<br>

When should you translate C code and when should you simply call C code from V?

If you have well-written, well-tested C code,
then of course you can always simply call this C code from V.

Translating it to V gives you several advantages:

- If you plan to develop that code base, you now have everything in one language,
    which is much safer and easier to develop in than C.
- Cross-compilation becomes a lot easier. You don't have to worry about it at all.
- No more build flags and include files either.

## Hot code reloading

```v live
module main

import time
import os

[live]
fn print_message() {
	println('Hello! Modify this message while the program is running.')
}

fn main() {
	for {
		print_message()
		time.sleep_ms(500)
	}
}
```

Build this example with `v -live message.v`.

Functions that you want to be reloaded must have `[live]` attribute
before their definition.

Right now it's not possible to modify types while the program is running.

More examples, including a graphical application:
[github.com/vlang/v/tree/master/examples/hot_code_reload](https://github.com/vlang/v/tree/master/examples/hot_reload).

## Cross compilation

To cross compile your project simply run

```shell
v -os windows .
```

or

```shell
v -os linux .
```

(Cross compiling for macOS is temporarily not possible.)

If you don't have any C dependencies, that's all you need to do. This works even
when compiling GUI apps using the `ui` module or graphical apps using `gg`.

You will need to install Clang, LLD linker, and download a zip file with
libraries and include files for Windows and Linux. V will provide you with a link.

## Cross-platform shell scripts in V

V can be used as an alternative to Bash to write deployment scripts, build scripts, etc.

The advantage of using V for this is the simplicity and predictability of the language, and
cross-platform support. "V scripts" run on Unix-like systems as well as on Windows.

Use the `.vsh` file extension. It will make all functions in the `os`
module global (so that you can use `ls()` instead of `os.ls()`, for example).

```v wip
#!/usr/local/bin/v run
// The shebang above associates the file to V on Unix-like systems,
// so it can be run just by specifying the path to the file
// once it's made executable using `chmod +x`.

rm('build/*')
// Same as:
for file in ls('build/') {
    rm(file)
}

mv('*.v', 'build/')
// Same as:
for file in ls('.') {
    if file.ends_with('.v') {
        mv(file, 'build/')
    }
}
```

Now you can either compile this like a normal V program and get an executable you can deploy and run
anywhere:
`v deploy.vsh && ./deploy`

Or just run it more like a traditional Bash script:
`v run deploy.vsh`

On Unix-like platforms, the file can be run directly after making it executable using `chmod +x`:
`./deploy.vsh`

## Attributes

V has several attributes that modify the behavior of functions and structs.

An attribute is specified inside `[]` right before a function/struct declaration
and applies only to the following declaration.

```v
// Calling this function will result in a deprecation warning
[deprecated]
fn old_function() {
}

// This function's calls will be inlined.
[inline]
fn inlined_function() {
}

// The following struct can only be used as a reference (`&Window`) and allocated on the heap.
[ref_only]
struct Window {
}

// V will not generate this function and all its calls if the provided flag is false.
// To use a flag, use `v -d flag`
[if debug]
fn foo() {
}

fn bar() {
	foo() // will not be called if `-d debug` is not passed
}

// For C interop only, tells V that the following struct is defined with `typedef struct` in C
[typedef]
struct C.Foo {
}

// Used in Win32 API code when you need to pass callback function
[windows_stdcall]
fn C.DefWindowProc(hwnd int, msg int, lparam int, wparam int)
```


# Appendices

## Appendix I: Keywords

V has 41 reserved keywords (3 are literals):

```v ignore
as
asm
assert
atomic
break
const
continue
defer
else
embed
enum
false
fn
for
go
goto
if
import
in
interface
is
lock
match
module
mut
none
or
pub
return
rlock
select
shared
sizeof
static
struct
true
type
typeof
union
unsafe
```
See also [Types](#types).

## Appendix II: Operators

This lists operators for [primitive types](#primitive-types) only.

```v ignore
+    sum                    integers, floats, strings
-    difference             integers, floats
*    product                integers, floats
/    quotient               integers, floats
%    remainder              integers

~    bitwise NOT            integers
&    bitwise AND            integers
|    bitwise OR             integers
^    bitwise XOR            integers

!    logical NOT            bools
&&   logical AND            bools
||   logical OR             bools
!=   logical XOR            bools

<<   left shift             integer << unsigned integer
>>   right shift            integer >> unsigned integer


Precedence    Operator
    5             *  /  %  <<  >>  &
    4             +  -  |  ^
    3             ==  !=  <  <=  >  >=
    2             &&
    1             ||


Assignment Operators
+=   -=   *=   /=   %=
&=   |=   ^=
>>=  <<=
```
