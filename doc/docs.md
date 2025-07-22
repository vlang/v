# V Documentation

(See https://modules.vlang.io/ for documentation of V's standard library)
(See also https://docs.vlang.io/introduction.html, which has the same information as this document,
but split in separate pages for each section, for easier reading on mobile devices)

## Introduction

V is a statically typed compiled programming language designed for building maintainable software.

It's similar to Go and its design has also been influenced by Oberon, Rust, Swift,
Kotlin, and Python.

V is a very simple language. Going through this documentation will take you about a weekend,
and by the end of it you will have pretty much learned the entire language.

The language promotes writing simple and clear code with minimal abstraction.

Despite being simple, V gives the developer a lot of power.
Anything you can do in other languages, you can do in V.

## Installing V from source

The best way to get the latest and greatest V, is to install it from source.
It is easy, and it takes only a few seconds:
```bash
git clone --depth=1 https://github.com/vlang/v
cd v
make
```

Note: If you are on windows, outside of WSL, run `make.bat` instead of `make`, in a CMD shell.
Note: On Ubuntu/Debian, you may need to run `sudo apt install git build-essential make` first.

For more details, see the
[Installing V](https://github.com/vlang/v/blob/master/README.md#installing-v-from-source)
section in the README.md.

## Upgrading V to latest version

If V is already installed on a machine, it can be upgraded to its latest version
by using the V's built-in self-updater.
To do so, run the command `v up`.

## Packaging V for distribution
See the [notes on how to prepare a package for V](packaging_v_for_distributions.md) .

## Getting started

You can let V automatically set up the bare-bones structure of a project for you
by using any of the following commands in a terminal:

* `v init` → adds necessary files to the current folder to make it a V project
* `v new abc` → creates a new project in the new folder `abc`, by default a "hello world" project.
* `v new --web abcd` → creates a new project in the new folder `abcd`, using the vweb template.

## Table of Contents

<table>
<tr><td width=33% valign=top>

* [Hello world](#hello-world)
* [Running a project folder](#running-a-project-folder-with-several-files)
* [Comments](#comments)
* [Functions](#functions)
    * [Hoisting](#hoisting)
    * [Returning multiple values](#returning-multiple-values)
* [Symbol visibility](#symbol-visibility)
* [Variables](#variables)
    * [Mutable variables](#mutable-variables)
    * [Initialization vs assignment](#initialization-vs-assignment)
    * [Warnings and declaration errors](#warnings-and-declaration-errors)
* [V types](#v-types)
    * [Primitive types](#primitive-types)
    * [Strings](#strings)
    * [Runes](#runes)
    * [Numbers](#numbers)
    * [Arrays](#arrays)
        * [Multidimensional arrays](#multidimensional-arrays)
        * [Array methods](#array-methods)
        * [Array slices](#array-slices)
    * [Fixed size arrays](#fixed-size-arrays)
    * [Maps](#maps)
        * [Map update syntax](#map-update-syntax)

</td><td width=33% valign=top>

* [Module imports](#module-imports)
    * [Selective imports](#selective-imports)
    * [Module hierarchy](#module-hierarchy)
    * [Module import aliasing](#module-import-aliasing)
* [Statements & expressions](#statements--expressions)
    * [If](#if)
        * [`If` expressions](#if-expressions)
        * [`If` unwrapping](#if-unwrapping)
    * [Match](#match)
    * [In operator](#in-operator)
    * [For loop](#for-loop)
    * [Defer](#defer)
    * [Goto](#goto)
* [Structs](#structs)
    * [Heap structs](#heap-structs)
    * [Default field values](#default-field-values)
    * [Required fields](#required-fields)
    * [Short struct literal syntax](#short-struct-literal-syntax)
    * [Struct update syntax](#struct-update-syntax)
    * [Trailing struct literal arguments](#trailing-struct-literal-arguments)
    * [Access modifiers](#access-modifiers)
    * [Anonymous structs](#anonymous-structs)
    * [Static type methods](#static-type-methods)
    * [[noinit] structs](#noinit-structs)
    * [Methods](#methods)
    * [Embedded structs](#embedded-structs)
* [Unions](#unions)
    * [Why use unions?](#why-use-unions)
    * [Embedding](#embedding)

</td><td valign=top>

* [Functions 2](#functions-2)
    * [Immutable function args by default](#immutable-function-args-by-default)
    * [Mutable arguments](#mutable-arguments)
    * [Variable number of arguments](#variable-number-of-arguments)
    * [Anonymous & higher-order functions](#anonymous--higher-order-functions)
    * [Lambda expressions](#lambda-expressions)
    * [Closures](#closures)
    * [Parameter evaluation order](#parameter-evaluation-order)
* [References](#references)
* [Constants](#constants)
    * [Required module prefix](#required-module-prefix)
* [Builtin functions](#builtin-functions)
    * [println](#println)
    * [Printing custom types](#printing-custom-types)
    * [Dumping expressions at runtime](#dumping-expressions-at-runtime)
* [Modules](#modules)
    * [Create modules](#create-modules)
    * [Special considerations for project folders](#special-considerations-for-project-folders)
    * [init functions](#init-functions)
    * [cleanup functions](#cleanup-functions)

</td></tr>
<tr><td width=33% valign=top>

* [Type Declarations](#type-declarations)
    * [Type aliases](#type-aliases)
    * [Enums](#enums)
    * [Function Types](#function-types)
    * [Interfaces](#interfaces)
    * [Sum types](#sum-types)
    * [Option/Result types & error handling](#optionresult-types-and-error-handling)
        * [Handling options/results](#handling-optionsresults)
    * [Custom error types](#custom-error-types)
    * [Generics](#generics)
* [Concurrency](#concurrency)
    * [Spawning Concurrent Tasks](#spawning-concurrent-tasks)
    * [Channels](#channels)
    * [Shared Objects](#shared-objects)
* [JSON](#json)
    * [Decoding JSON](#decoding-json)
    * [Encoding JSON](#encoding-json)
* [Testing](#testing)
    * [Asserts](#asserts)
    * [Asserts with an extra message](#asserts-with-an-extra-message)
    * [Asserts that do not abort your program](#asserts-that-do-not-abort-your-program)
    * [Test files](#test-files)
    * [Running tests](#running-tests)
* [Memory management](#memory-management)
    * [Control](#control)
    * [Stack and Heap](#stack-and-heap)
* [ORM](#orm)
* [Writing documentation](#writing-documentation)
    * [Newlines in Documentation Comments](#newlines-in-documentation-comments)

</td><td width=33% valign=top>

* [Tools](#tools)
    * [v fmt](#v-fmt)
    * [v shader](#v-shader)
    * [Profiling](#profiling)
* [Package Management](#package-management)
    * [Package commands](#package-commands)
    * [Publish package](#publish-package)
* [Advanced Topics](#advanced-topics)
    * [Attributes](#attributes)
    * [Conditional compilation](#conditional-compilation)
        * [Compile time pseudo variables](#compile-time-pseudo-variables)
        * [Compile time reflection](#compile-time-reflection)
        * [Compile time code](#compile-time-code)
        * [Compile time types](#compile-time-types)
        * [Environment specific files](#environment-specific-files)
	* [Debugger](#debugger)
 		* [Call stack](#call-stack)
   		* [Trace](#trace)
    * [Memory-unsafe code](#memory-unsafe-code)
    * [Structs with reference fields](#structs-with-reference-fields)
    * [sizeof and __offsetof](#sizeof-and-__offsetof)
    * [Limited operator overloading](#limited-operator-overloading)
    * [Performance tuning](#performance-tuning)
    * [Atomics](#atomics)
    * [Global Variables](#global-variables)
    * [Static Variables](#static-variables)
    * [Cross compilation](#cross-compilation)
    * [Debugging](#debugging)
        * [C Backend binaries Default](#c-backend-binaries-default)
        * [Native Backend binaries](#native-backend-binaries)
        * [Javascript Backend](#javascript-backend)

</td><td valign=top>

* [V and C](#v-and-c)
    * [Calling C from V](#calling-c-from-v)
    * [Calling V from C](#calling-v-from-c)
    * [Passing C compilation flags](#passing-c-compilation-flags)
    * [#pkgconfig](#pkgconfig)
    * [Including C code](#including-c-code)
    * [C types](#c-types)
    * [C Declarations](#c-declarations)
    * [Export to shared library](#export-to-shared-library)
    * [Translating C to V](#translating-c-to-v)
    * [Working around C issues](#working-around-c-issues)
* [Other V Features](#other-v-features)
    * [Inline assembly](#inline-assembly)
    * [Hot code reloading](#hot-code-reloading)
    * [Cross-platform shell scripts in V](#cross-platform-shell-scripts-in-v)
    * [Vsh scripts with no extension](#vsh-scripts-with-no-extension)
* [Appendices](#appendices)
    * [Keywords](#appendix-i-keywords)
    * [Operators](#appendix-ii-operators)
    * [Other online resources](#other-online-resources)

</td></tr>
</table>

<!--
Note: There are several special keywords, which you can put after the code fences for v:
compile, cgen, live, ignore, failcompile, okfmt, oksyntax, badsyntax, wip, nofmt
For more details, do: `v check-md`
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
> If you haven't yet, you have to type the path to V manually.

Congratulations - you just wrote and executed your first V program!

You can compile a program without execution with `v hello.v`.
See `v help` for all supported commands.

From the example above, you can see that functions are declared with the `fn` keyword.
The return type is specified after the function name.
In this case `main` doesn't return anything, so there is no return type.

As in many other languages (such as C, Go, and Rust), `main` is the entry point of your program.

[`println`](#println) is one of the few [built-in functions](#builtin-functions).
It prints the value passed to it to standard output.

`fn main()` declaration can be skipped in single file programs.
This is useful when writing small programs, "scripts", or just learning the language.
For brevity, `fn main()` will be skipped in this tutorial.

This means that a "hello world" program in V is as simple as

```v
println('hello world')
```

> [!NOTE]
> If you do not explicitly use `fn main() {}`, you need to make sure that all your
> declarations come before any variable assignment statements or top level function calls,
> since V will consider everything after the first assignment/function call as part of your
> implicit main function.

## Running a project folder with several files

Suppose you have a folder with several .v files in it, where one of them
contains your `main()` function, and the other files have other helper
functions. They may be organized by topic, but still *not yet* structured
enough to be their own separate reusable modules, and you want to compile
them all into one program.

In other languages, you would have to use includes or a build system
to enumerate all files, compile them separately to object files,
then link them into one final executable.

In V however, you can compile and run the whole folder of .v files together,
using just `v run .`. Passing parameters also works, so you can
do: `v run . --yourparam some_other_stuff`

The above will first compile your files into a single program (named
after your folder/project), and then it will execute the program with
`--yourparam some_other_stuff` passed to it as CLI parameters.

Your program can then use the CLI parameters like this:

```v
import os

println(os.args)
```

> [!NOTE]
> After a successful run, V will delete the generated executable.
> If you want to keep it, use `v -keepc run .` instead, or just compile
> manually with `v .` .

> [!NOTE]
> Any V compiler flags should be passed *before* the `run` command.
> Everything after the source file/folder, will be passed to the program
> as is - it will not be processed by V.

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

### Hoisting

Functions can be used before their declaration:
`add` and `sub` are declared after `main`, but can still be called from `main`.
This is true for all declarations in V and eliminates the need for header files
or thinking about the order of files and declarations.

### Returning multiple values

```v
fn foo() (int, int) {
	return 2, 3
}

a, b := foo()
println(a) // 2
println(b) // 3
c, _ := foo() // ignore values using `_`
```

## Symbol visibility

```v
pub fn public_function() {
}

fn private_function() {
}
```

Functions are private (not exported) by default.
To allow other [modules](#module-imports) to use them, prepend `pub`. The same applies
to [structs](#structs), [constants](#constants) and [types](#type-declarations).

> [!NOTE]
> `pub` can only be used from a named module.
> For information about creating a module, see [Modules](#modules).

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
By default V does not allow **global variables**. See more [details](#global-variables).

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

The values of multiple variables can be changed in one line.
In this way, their values can be swapped without an intermediary variable.

```v
mut a := 0
mut b := 1
println('${a}, ${b}') // 0, 1
a, b = b, a
println('${a}, ${b}') // 1, 0
```

### Warnings and declaration errors

In development mode the compiler will warn you that you haven't used the variable
(you'll get an "unused variable" warning).
In production mode (enabled by passing the `-prod` flag to v – `v -prod foo.v`)
it will not compile at all (like in Go).
```v
fn main() {
	a := 10
	// warning: unused variable `a`
}
```

To ignore values returned by a function `_` can be used
```v
fn foo() (int, int) {
	return 2, 3
}

fn main() {
	c, _ := foo()
	print(c)
	// no warning about unused variable returned by foo.
}
```

Unlike most languages, variable shadowing is not allowed. Declaring a variable with a name
that is already used in a parent scope will cause a compilation error.
```v failcompile nofmt
fn main() {
	a := 10
	{
		a := 20 // error: redefinition of `a`
	}
}
```
While variable shadowing is not allowed, field shadowing is allowed.
```v
pub struct Dimension {
	width  int = -1
	height int = -1
}

pub struct Test {
	Dimension
	width int = 100
	// height int
}

fn main() {
	test := Test{}
	println('${test.width} ${test.height} ${test.Dimension.width}') // 100 -1 -1
}
```
## V Types

### Primitive types

```v ignore
bool

string

i8    i16  int  i64      i128 (soon)
u8    u16  u32  u64      u128 (soon)

rune // represents a Unicode code point

f32 f64

isize, usize // platform-dependent, the size is how many bytes it takes to reference any location in memory

voidptr // this one is mostly used for [C interoperability](#v-and-c)
```

> [!NOTE]
> Unlike C and Go, `int` is always a 32 bit integer.

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
   u8 → u16 → u32 → u64 ⬎
      ↘     ↘     ↘      ptr
   i8 → i16 → int → i64 ⬏
```

An `int` value for example can be automatically promoted to `f64`
or `i64` but not to `u32`. (`u32` would mean loss of the sign for
negative values).
Promotion from `int` to `f32`, however, is currently done automatically
(but can lead to precision loss for large values).

Literals like `123` or `4.56` are treated in a special way. They do
not lead to type promotions, however they default to `int` and `f64`
respectively, when their type has to be decided:

```v nofmt
u := u16(12)
v := 13 + u    // v is of type `u16` - no promotion
x := f32(45.6)
y := x + 3.14  // y is of type `f32` - no promotion
a := 75        // a is of type `int` - default for int literal
b := 14.7      // b is of type `f64` - default for float literal
c := u + a     // c is of type `int` - automatic promotion of `u`'s value
d := b + x     // d is of type `f64` - automatic promotion of `x`'s value
```

### Strings

In V, strings are encoded in UTF-8, and are immutable (read-only) by default:

```v
s := 'hello 🌎' // the `world` emoji takes 4 bytes, and string length is reported in bytes
assert s.len == 10

arr := s.bytes() // convert `string` to `[]u8`
assert arr.len == 10

s2 := arr.bytestr() // convert `[]u8` to `string`
assert s2 == s

name := 'Bob'
assert name.len == 3
// indexing gives a byte, u8(66) == `B`
assert name[0] == u8(66)
// slicing gives a string 'ob'
assert name[1..3] == 'ob'

// escape codes
// escape special characters like in C
windows_newline := '\r\n'
assert windows_newline.len == 2

// arbitrary bytes can be directly specified using `\x##` notation where `#` is
// a hex digit
aardvark_str := '\x61ardvark'
assert aardvark_str == 'aardvark'
assert '\xc0'[0] == u8(0xc0)

// or using octal escape `\###` notation where `#` is an octal digit
aardvark_str2 := '\141ardvark'
assert aardvark_str2 == 'aardvark'

// Unicode can be specified directly as `\u####` where # is a hex digit
// and will be converted internally to its UTF-8 representation
star_str := '\u2605' // ★
assert star_str == '★'
// UTF-8 can be specified this way too, as individual bytes.
assert star_str == '\xe2\x98\x85'
```

Since strings are immutable, you cannot directly change characters in a string:

```v failcompile
mut s := 'hello 🌎'
s[0] = `H` // not allowed
```

> error: cannot assign to `s[i]` since V strings are immutable

Note that indexing a string normally will produce a `u8` (byte), not a `rune` nor another `string`.
Indexes correspond to _bytes_ in the string, not Unicode code points.
If you want to convert the `u8` to a `string`, use the `.ascii_str()` method on the `u8`:

```v
country := 'Netherlands'
println(country[0]) // Output: 78
println(country[0].ascii_str()) // Output: N
```

However, you can easily get the runes for a string with the `runes()` method, which will return an
array of the UTF-8 characters from the string.  You can then index this array.  Just be aware that
there may be fewer indexes available on the `rune` array than on the bytes in the string, if there
_are_ any non-ASCII characters.

```v
mut s := 'hello 🌎'
// there are 10 bytes in the string (as shown earlier), but only 7 runes, since the `world` emoji
// only counts as one `rune` (one Unicode character)
assert s.runes().len == 7
println(s.runes()[6])
```

If you want the code point from a specific `string` index or other more advanced UTF-8 processing
and conversions, refer to the
[vlib/encoding/utf8](https://modules.vlang.io/encoding.utf8.html) module.

Both single and double quotes can be used to denote strings. For consistency, `vfmt` converts double
quotes to single quotes unless the string contains a single quote character.

Prepend `r` for raw strings. Escapes are not handled, so you will get exacly what you type:

```v
s := r'hello\nworld' // the `\n` will be preserved as two characters
println(s) // "hello\nworld"
```

Strings can be easily converted to integers:

```v
s := '42'
n := s.int() // 42

// all int literals are supported
assert '0xc3'.int() == 195
assert '0o10'.int() == 8
assert '0b1111_0000_1010'.int() == 3850
assert '-0b1111_0000_1010'.int() == -3850
```

For more advanced `string` processing and conversions, refer to the
[vlib/strconv](https://modules.vlang.io/strconv.html) module.

#### String interpolation

Basic interpolation syntax is pretty simple - use `${` before a variable name and `}` after. The
variable will be converted to a string and embedded into the literal:

```v
name := 'Bob'
println('Hello, ${name}!') // Hello, Bob!
```

It also works with fields: `'age = ${user.age}'`. You may also use more complex expressions:
`'can register = ${user.age > 13}'`.

Format specifiers similar to those in C's `printf()` are also supported. `f`, `g`, `x`, `o`, `b`,
etc. are optional and specify the output format. The compiler takes care of the storage size, so
there is no `hd` or `llu`.

To use a format specifier, follow this pattern:

`${varname:[flags][width][.precision][type]}`

- flags: may be zero or more of the following: `-` to left-align output within the field, `0` to use
  `0` as the padding character instead of the default `space` character.
  > **Note**
  >
  > V does not currently support the use of `'` or `#` as format flags, and V supports but
  > doesn't need `+` to right-align since that's the default.
- width: may be an integer value describing the minimum width of total field to output.
- precision: an integer value preceded by a `.` will guarantee that many digits after the decimal
  point without any insignificant trailing zeros. If displaying insignificant zero's is desired,
  append a `f` specifier to the precision value (see examples below). Applies only to float
  variables and is ignored for integer variables.
- type: `f` and `F` specify the input is a float and should be rendered as such, `e` and `E` specify
  the input is a float and should be rendered as an exponent (partially broken), `g` and `G` specify
  the input is a float--the renderer will use floating point notation for small values and exponent
  notation for large values, `d` specifies the input is an integer and should be rendered in base-10
  digits, `x` and `X` require an integer and will render it as hexadecimal digits, `o` requires an
  integer and will render it as octal digits, `b` requires an integer and will render it as binary
  digits, `s` requires a string (almost never used).

  > **Note**
  >
  > When a numeric type can render alphabetic characters, such as hex strings or special values
  > like `infinity`, the lowercase version of the type forces lowercase alphabetics and the
  > uppercase version forces uppercase alphabetics.

  > **Note**
  >
  > In most cases, it's best to leave the format type empty. Floats will be rendered by
  > default as `g`, integers will be rendered by default as `d`, and `s` is almost always redundant.
  > There are only three cases where specifying a type is recommended:

- format strings are parsed at compile time, so specifying a type can help detect errors then
- format strings default to using lowercase letters for hex digits and the `e` in exponents. Use a
  uppercase type to force the use of uppercase hex digits and an uppercase `E` in exponents.
- format strings are the most convenient way to get hex, binary or octal strings from an integer.

See
[Format Placeholder Specification](https://en.wikipedia.org/wiki/Printf_format_string#Format_placeholder_specification)
for more information.

```v
x := 123.4567
println('[${x:.2}]') // round to two decimal places => [123.46]
println('[${x:10}]') // right-align with spaces on the left => [   123.457]
println('[${int(x):-10}]') // left-align with spaces on the right => [123       ]
println('[${int(x):010}]') // pad with zeros on the left => [0000000123]
println('[${int(x):b}]') // output as binary => [1111011]
println('[${int(x):o}]') // output as octal => [173]
println('[${int(x):X}]') // output as uppercase hex => [7B]

println('[${10.0000:.2}]') // remove insignificant 0s at the end => [10]
println('[${10.0000:.2f}]') // do show the 0s at the end, even though they do not change the number => [10.00]
```

V also has `r` and `R` switches, which will repeat the string the specified amount of times.

```v
println('[${'abc':3r}]') // [abcabcabc]
println('[${'abc':3R}]') // [ABCABCABC]
```

#### String operators

```v
name := 'Bob'
bobby := name + 'by' // + is used to concatenate strings
println(bobby) // "Bobby"
mut s := 'hello '
s += 'world' // `+=` is used to append to a string
println(s) // "hello world"
```

All operators in V must have values of the same type on both sides. You cannot concatenate an
integer to a string:

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
println('age = ${age}')
```

See all methods of [string](https://modules.vlang.io/index.html#string)
and related modules [strings](https://modules.vlang.io/strings.html),
[strconv](https://modules.vlang.io/strconv.html).

### Runes

A `rune` represents a single UTF-32 encoded Unicode character and is an alias for `u32`.
To denote them, use <code>`</code> (backticks) :

```v
rocket := `🚀`
```

A `rune` can be converted to a UTF-8 string by using the `.str()` method.

```v
rocket := `🚀`
assert rocket.str() == '🚀'
```

A `rune` can be converted to UTF-8 bytes by using the `.bytes()` method.

```v
rocket := `🚀`
assert rocket.bytes() == [u8(0xf0), 0x9f, 0x9a, 0x80]
```

Hex, Unicode, and Octal escape sequences also work in a `rune` literal:

```v
assert `\x61` == `a`
assert `\141` == `a`
assert `\u0061` == `a`

// multibyte literals work too
assert `\u2605` == `★`
assert `\u2605`.bytes() == [u8(0xe2), 0x98, 0x85]
assert `\xe2\x98\x85`.bytes() == [u8(0xe2), 0x98, 0x85]
assert `\342\230\205`.bytes() == [u8(0xe2), 0x98, 0x85]
```

Note that `rune` literals use the same escape syntax as strings, but they can only hold one unicode
character. Therefore, if your code does not specify a single Unicode character, you will receive an
error at compile time.

Also remember that strings are indexed as bytes, not runes, so beware:

```v
rocket_string := '🚀'
assert rocket_string[0] != `🚀`
assert 'aloha!'[0] == `a`
```

A string can be converted to runes by the `.runes()` method.

```v
hello := 'Hello World 👋'
hello_runes := hello.runes() // [`H`, `e`, `l`, `l`, `o`, ` `, `W`, `o`, `r`, `l`, `d`, ` `, `👋`]
assert hello_runes.string() == hello
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

```v
num := 1_000_000 // same as 1000000
three := 0b0_11 // same as 0b11
float_num := 3_122.55 // same as 3122.55
hexa := 0xF_F // same as 255
oct := 0o17_3 // same as 0o173
```

If you want a different type of integer, you can use casting:

```v
a := i64(123)
b := u8(42)
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

Float literals can also be declared as a power of ten:

```v
f0 := 42e1 // 420
f1 := 123e-2 // 1.23
f2 := 456e+2 // 45600
```

### Arrays

An array is a collection of data elements of the same type. An array literal is a
list of expressions surrounded by square brackets. An individual element can be
accessed using an *index* expression. Indexes start from `0`:

```v
mut nums := [1, 2, 3]
println(nums) // `[1, 2, 3]`
println(nums[0]) // `1`
println(nums[1]) // `2`

nums[1] = 5
println(nums) // `[1, 5, 3]`
```

<a id='array-operations'></a>

An element can be appended to the end of an array using the push operator `<<`.
It can also append an entire array.

```v
mut nums := [1, 2, 3]
nums << 4
println(nums) // "[1, 2, 3, 4]"

// append array
nums << [5, 6, 7]
println(nums) // "[1, 2, 3, 4, 5, 6, 7]"
```

```v
mut names := ['John']
names << 'Peter'
names << 'Sam'
// names << 10  <-- This will not compile. `names` is an array of strings.
```

`val in array` returns true if the array contains `val`. See [`in` operator](#in-operator).

```v
names := ['John', 'Peter', 'Sam']
println('Alex' in names) // "false"
```

#### Array Fields

There are two fields that control the "size" of an array:

* `len`: *length* - the number of pre-allocated and initialized elements in the array
* `cap`: *capacity* - the amount of memory space which has been reserved for elements,
  but not initialized or counted as elements. The array can grow up to this size without
  being reallocated. Usually, V takes care of this field automatically but there are
  cases where the user may want to do manual optimizations (see [below](#array-initialization)).

```v
mut nums := [1, 2, 3]
println(nums.len) // "3"
println(nums.cap) // "3" or greater
nums = [] // The array is now empty
println(nums.len) // "0"
```

`data` is a field (of type `voidptr`) with the address of the first
element. This is for low-level [`unsafe`](#memory-unsafe-code) code.

> [!NOTE]
> Fields are read-only and can't be modified by the user.

#### Array Initialization

The type of an array is determined by the first element:

* `[1, 2, 3]` is an array of ints (`[]int`).
* `['a', 'b']` is an array of strings (`[]string`).

The user can explicitly specify the type for the first element: `[u8(16), 32, 64, 128]`.
V arrays are homogeneous (all elements must have the same type).
This means that code like `[1, 'a']` will not compile.

The above syntax is fine for a small number of known elements but for very large or empty
arrays there is a second initialization syntax:

```v
mut a := []int{len: 10000, cap: 30000, init: 3}
```

This creates an array of 10000 `int` elements that are all initialized with `3`. Memory
space is reserved for 30000 elements. The parameters `len`, `cap` and `init` are optional;
`len` defaults to `0` and `init` to the default initialization of the element type (`0`
for numerical type, `''` for `string`, etc). The run time system makes sure that the
capacity is not smaller than `len` (even if a smaller value is specified explicitly):

```v
arr := []int{len: 5, init: -1}
// `arr == [-1, -1, -1, -1, -1]`, arr.cap == 5

// Declare an empty array:
users := []int{}
```

Setting the capacity improves performance of pushing elements to the array
as reallocations can be avoided:

```v
mut numbers := []int{cap: 1000}
println(numbers.len) // 0
// Now appending elements won't reallocate
for i in 0 .. 1000 {
	numbers << i
}
```

> [!NOTE]
> The above code uses a [range `for`](#range-for) statement.

You can initialize the array by accessing the `index` variable which gives
the index as shown here:

```v
count := []int{len: 4, init: index}
assert count == [0, 1, 2, 3]

mut square := []int{len: 6, init: index * index}
// square == [0, 1, 4, 9, 16, 25]
```

#### Array Types

An array can be of these types:

| Types        | Example Definition                   |
|--------------|--------------------------------------|
| Number       | `[]int,[]i64`                        |
| String       | `[]string`                           |
| Rune         | `[]rune`                             |
| Boolean      | `[]bool`                             |
| Array        | `[][]int`                            |
| Struct       | `[]MyStructName`                     |
| Channel      | `[]chan f64`                         |
| Function     | `[]MyFunctionType` `[]fn (int) bool` |
| Interface    | `[]MyInterfaceName`                  |
| Sum Type     | `[]MySumTypeName`                    |
| Generic Type | `[]T`                                |
| Map          | `[]map[string]f64`                   |
| Enum         | `[]MyEnumType`                       |
| Alias        | `[]MyAliasTypeName`                  |
| Thread       | `[]thread int`                       |
| Reference    | `[]&f64`                             |
| Shared       | `[]shared MyStructType`              |
| Option       | `[]?f64`                          |

**Example Code:**

This example uses [Structs](#structs) and [Sum Types](#sum-types) to create an array
which can handle different types (e.g. Points, Lines) of data elements.

```v
struct Point {
	x int
	y int
}

struct Line {
	p1 Point
	p2 Point
}

type ObjectSumType = Line | Point

mut object_list := []ObjectSumType{}
object_list << Point{1, 1}
object_list << Line{
	p1: Point{3, 3}
	p2: Point{4, 4}
}
dump(object_list)
/*
object_list: [ObjectSumType(Point{
    x: 1
    y: 1
}), ObjectSumType(Line{
    p1: Point{
        x: 3
        y: 3
    }
    p2: Point{
        x: 4
        y: 4
    }
})]
*/
```

#### Multidimensional Arrays

Arrays can have more than one dimension.

2d array example:

```v
mut a := [][]int{len: 2, init: []int{len: 3}}
a[0][1] = 2
println(a) // [[0, 2, 0], [0, 0, 0]]
```

3d array example:

```v
mut a := [][][]int{len: 2, init: [][]int{len: 3, init: []int{len: 2}}}
a[0][1][1] = 2
println(a) // [[[0, 0], [0, 2], [0, 0]], [[0, 0], [0, 0], [0, 0]]]
```

#### Array methods

All arrays can be easily printed with `println(arr)` and converted to a string
with `s := arr.str()`.

Copying the data from the array is done with `.clone()`:

```v
nums := [1, 2, 3]
nums_copy := nums.clone()
```

Arrays can be efficiently filtered and mapped with the `.filter()` and
`.map()` methods:

```v
nums := [1, 2, 3, 4, 5, 6]
even := nums.filter(it % 2 == 0)
println(even) // [2, 4, 6]
// filter can accept anonymous functions
even_fn := nums.filter(fn (x int) bool {
	return x % 2 == 0
})
println(even_fn)
```

```v
words := ['hello', 'world']
upper := words.map(it.to_upper())
println(upper) // ['HELLO', 'WORLD']
// map can also accept anonymous functions
upper_fn := words.map(fn (w string) string {
	return w.to_upper()
})
println(upper_fn) // ['HELLO', 'WORLD']
```

`it` is a builtin variable which refers to the element currently being
processed in filter/map methods.

Additionally, `.any()` and `.all()` can be used to conveniently test
for elements that satisfy a condition.

```v
nums := [1, 2, 3]
println(nums.any(it == 2)) // true
println(nums.all(it >= 2)) // false
```

There are further built-in methods for arrays:

* `a.repeat(n)` concatenates the array elements `n` times
* `a.insert(i, val)` inserts a new element `val` at index `i` and
  shifts all following elements to the right
* `a.insert(i, [3, 4, 5])` inserts several elements
* `a.prepend(val)` inserts a value at the beginning, equivalent to `a.insert(0, val)`
* `a.prepend(arr)` inserts elements of array `arr` at the beginning
* `a.trim(new_len)` truncates the length (if `new_length < a.len`, otherwise does nothing)
* `a.clear()` empties the array without changing `cap` (equivalent to `a.trim(0)`)
* `a.delete_many(start, size)` removes `size` consecutive elements from index `start`
  &ndash; triggers reallocation
* `a.delete(index)` equivalent to `a.delete_many(index, 1)`
* `a.delete_last()` removes the last element
* `a.first()` equivalent to `a[0]`
* `a.last()` equivalent to `a[a.len - 1]`
* `a.pop()` removes the last element and returns it
* `a.reverse()` makes a new array with the elements of `a` in reverse order
* `a.reverse_in_place()` reverses the order of elements in `a`
* `a.join(joiner)` concatenates an array of strings into one string
  using `joiner` string as a separator

See all methods of [array](https://modules.vlang.io/index.html#array)

See also [vlib/arrays](https://modules.vlang.io/arrays.html).

##### Sorting Arrays

Sorting arrays of all kinds is very simple and intuitive. Special variables `a` and `b`
are used when providing a custom sorting condition.

```v
mut numbers := [1, 3, 2]
numbers.sort() // 1, 2, 3
numbers.sort(a > b) // 3, 2, 1
```

```v
struct User {
	age  int
	name string
}

mut users := [User{21, 'Bob'}, User{20, 'Zarkon'}, User{25, 'Alice'}]
users.sort(a.age < b.age) // sort by User.age int field
users.sort(a.name > b.name) // reverse sort by User.name string field
```

V also supports custom sorting, through the `sort_with_compare` array method.
Which expects a comparing function which will define the sort order.
Useful for sorting on multiple fields at the same time by custom sorting rules.
The code below sorts the array ascending on `name` and descending `age`.

```v
struct User {
	age  int
	name string
}

mut users := [User{21, 'Bob'}, User{65, 'Bob'}, User{25, 'Alice'}]

custom_sort_fn := fn (a &User, b &User) int {
	// return -1 when a comes before b
	// return 0, when both are in same order
	// return 1 when b comes before a
	if a.name == b.name {
		if a.age < b.age {
			return 1
		}
		if a.age > b.age {
			return -1
		}
		return 0
	}
	if a.name < b.name {
		return -1
	} else if a.name > b.name {
		return 1
	}
	return 0
}
users.sort_with_compare(custom_sort_fn)
```

#### Array Slices

A slice is a part of a parent array. Initially it refers to the elements
between two indices separated by a `..` operator. The right-side index must
be greater than or equal to the left side index.

If a right-side index is absent, it is assumed to be the array length. If a
left-side index is absent, it is assumed to be 0.

```v
nums := [0, 10, 20, 30, 40]
println(nums[1..4]) // [10, 20, 30]
println(nums[..4]) // [0, 10, 20, 30]
println(nums[1..]) // [10, 20, 30, 40]
```

In V slices are arrays themselves (they are not distinct types). As a result
all array operations may be performed on them. E.g. they can be pushed onto an
array of the same type:

```v
array_1 := [3, 5, 4, 7, 6]
mut array_2 := [0, 1]
array_2 << array_1[..3]
println(array_2) // `[0, 1, 3, 5, 4]`
```

A slice is always created with the smallest possible capacity `cap == len` (see
[`cap` above](#array-initialization)) no matter what the capacity or length
of the parent array is. As a result it is immediately reallocated and copied to another
memory location when the size increases thus becoming independent from the
parent array (*copy on grow*). In particular pushing elements to a slice
does not alter the parent:

```v
mut a := [0, 1, 2, 3, 4, 5]

// Create a slice, that reuses the *same memory* as the parent array
// initially, without doing a new allocation:
mut b := unsafe { a[2..4] } // the contents of `b`, reuses the memory, used by the contents of `a`.

b[0] = 7 // Note that `b[0]` and `a[2]` refer to *the same element* in memory.
println(a) // `[0, 1, 7, 3, 4, 5]` - changing `b[0]` above, changed `a[2]` too.

// the content of `b` will get reallocated, to have room for the `9` element:
b << 9
// The content of `b`, is now reallocated, and fully independent from the content of `a`.

println(a) // `[0, 1, 7, 3, 4, 5]` - no change, since the content of `b` was reallocated,
// to a larger block, before the appending.

println(b) // `[7, 3, 9]` - the contents of `b`, after the reallocation, and appending of the `9`.
```

Appending to the parent array, may or may not make it independent from its child slices.
The behaviour depends on the *parent's capacity* and is predictable:

```v
mut a := []int{len: 5, cap: 6, init: 2}
mut b := unsafe { a[1..4] } // the contents of `b` uses part of the same memory, that is used by `a` too

a << 3
// still no reallocation of `a`, since `a.len` still fits in `a.cap`
b[2] = 13 // `a[3]` is modified, through the slice `b`.

a << 4
// the content of `a` has been reallocated now, and is independent from `b` (`cap` was exceeded by `len`)
b[1] = 3 // no change in `a`

println(a) // `[2, 2, 2, 13, 2, 3, 4]`
println(b) // `[2, 3, 13]`
```

You can call .clone() on the slice, if you *do* want to have an independent copy right away:

```v
mut a := [0, 1, 2, 3, 4, 5]
mut b := a[2..4].clone()
b[0] = 7 // Note: `b[0]` is NOT referring to `a[2]`, as it would have been, without the `.clone()`
println(a) // [0, 1, 2, 3, 4, 5]
println(b) // [7, 3]
```

##### Slices with negative indexes

V supports array and string slices with negative indexes.
Negative indexing starts from the end of the array towards the start,
for example `-3` is equal to `array.len - 3`.
Negative slices have a different syntax from normal slices, i.e. you need
to add a `gate` between the array name and the square bracket: `a#[..-3]`.
The `gate` specifies that this is a different type of slice and remember that
the result is "locked" inside the array.
The returned slice is always a valid array, though it may be empty:

```v
a := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
println(a#[-3..]) // [7, 8, 9]
println(a#[-20..]) // [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
println(a#[-20..-8]) // [0, 1]
println(a#[..-3]) // [0, 1, 2, 3, 4, 5, 6]

// empty arrays
println(a#[-20..-10]) // []
println(a#[20..10]) // []
println(a#[20..30]) // []
```

#### Array method chaining

You can chain the calls of array methods like `.filter()` and `.map()` and use
the `it` built-in variable to achieve a classic `map/filter` functional paradigm:

```v
// using filter, map and negatives array slices
files := ['pippo.jpg', '01.bmp', '_v.txt', 'img_02.jpg', 'img_01.JPG']
filtered := files.filter(it#[-4..].to_lower() == '.jpg').map(it.to_upper())
// ['PIPPO.JPG', 'IMG_02.JPG', 'IMG_01.JPG']
```

### Fixed size arrays

V also supports arrays with fixed size. Unlike ordinary arrays, their
length is constant. You cannot append elements to them, nor shrink them.
You can only modify their elements in place.

However, access to the elements of fixed size arrays is more efficient,
they need less memory than ordinary arrays, and unlike ordinary arrays,
their data is on the stack, so you may want to use them as buffers if you
do not want additional heap allocations.

Most methods are defined to work on ordinary arrays, not on fixed size arrays.
You can convert a fixed size array to an ordinary array with slicing:

```v
mut fnums := [3]int{} // fnums is a fixed size array with 3 elements.
fnums[0] = 1
fnums[1] = 10
fnums[2] = 100
println(fnums) // => [1, 10, 100]
println(typeof(fnums).name) // => [3]int

fnums2 := [1, 10, 100]! // short init syntax that does the same (the syntax will probably change)

anums := fnums[..] // same as `anums := fnums[0..fnums.len]`
println(anums) // => [1, 10, 100]
println(typeof(anums).name) // => []int
```

Note that slicing will cause the data of the fixed size array to be copied to
the newly created ordinary array.

### Maps

```v
mut m := map[string]int{} // a map with `string` keys and `int` values
m['one'] = 1
m['two'] = 2
println(m['one']) // "1"
println(m['bad_key']) // "0"
println('bad_key' in m) // Use `in` to detect whether such key exists
println(m.keys()) // ['one', 'two']
m.delete('two')
```

Maps can have keys of type string, rune, integer, float or voidptr.

The whole map can be initialized using this short syntax:

```v
numbers := {
	'one': 1
	'two': 2
}
println(numbers)
```

If a key is not found, a zero value is returned by default:

```v
sm := {
	'abc': 'xyz'
}
val := sm['bad_key']
println(val) // ''
```

```v
intm := {
	1: 1234
	2: 5678
}
s := intm[3]
println(s) // 0
```

It's also possible to use an `or {}` block to handle missing keys:

```v
mm := map[string]int{}
val := mm['bad_key'] or { panic('key not found') }
```

You can also check, if a key is present, and get its value, if it was present, in one go:

```v
m := {
	'abc': 'def'
}
if v := m['abc'] {
	println('the map value for that key is: ${v}')
}
```

The same option check applies to arrays:

```v
arr := [1, 2, 3]
large_index := 999
val := arr[large_index] or { panic('out of bounds') }
println(val)
// you can also do this, if you want to *propagate* the access error:
val2 := arr[333]!
println(val2)
```

V also supports nested maps:

```v
mut m := map[string]map[string]int{}
m['greet'] = {
	'Hello': 1
}
m['place'] = {
	'world': 2
}
m['code']['orange'] = 123
print(m)
```

Maps are ordered by insertion, like dictionaries in Python. The order is a
guaranteed language feature. This may change in the future.

See all methods of
[map](https://modules.vlang.io/builtin.html#map)
and
[maps](https://modules.vlang.io/maps.html).

### Map update syntax

As with structs, V lets you initialise a map with an update applied on top of
another map:

```v
const base_map = {
	'a': 4
	'b': 5
}

foo := {
	...base_map
	'b': 88
	'c': 99
}

println(foo) // {'a': 4, 'b': 88, 'c': 99}
```

This is functionally equivalent to cloning the map and updating it, except that
you don't have to declare a mutable variable:

```v failcompile
// same as above (except mutable)
mut foo := base_map.clone()
foo['b'] = 88
foo['c'] = 99
```

## Module imports

For information about creating a module, see [Modules](#modules).

Modules can be imported using the `import` keyword:

```v
import os

fn main() {
	// read text from stdin
	name := os.input('Enter your name: ')
	println('Hello, ${name}!')
}
```

This program can use any public definitions from the `os` module, such
as the `input` function. See the [standard library](https://modules.vlang.io/)
documentation for a list of common modules and their public symbols.

By default, you have to specify the module prefix every time you call an external function.
This may seem verbose at first, but it makes code much more readable
and easier to understand - it's always clear which function from
which module is being called. This is especially useful in large code bases.

Cyclic module imports are not allowed, like in Go.

### Selective imports

You can also import specific functions and types from modules directly:

```v
import os { input }

fn main() {
	// read text from stdin
	name := input('Enter your name: ')
	println('Hello, ${name}!')
}
```

> [!NOTE]
> This will import the module as well. Also, this is not allowed for
> constants - they must always be prefixed.

You can import several specific symbols at once:

```v
import os { input, user_os }

name := input('Enter your name: ')
println('Name: ${name}')
current_os := user_os()
println('Your OS is ${current_os}.')
```
### Module hierarchy

> [!NOTE]
> This section is valid when .v files are not in the project's root directory.

Modules names in .v files, must match the name of their directory.

A .v file `./abc/source.v` must start with `module abc`. All .v files in this directory
belong to the same module `abc`. They should also start with `module abc`.

If you have `abc/def/`, and .v files in both folders, you can `import abc`, but you will have
to `import abc.def` too, to get to the symbols in the subfolder. It is independent.

In `module name` statement, name never repeats directory's hierarchy, but only its directory.
So in `abc/def/source.v` the first line will be `module def`, and not `module abc.def`.

`import module_name` statements must respect file hierarchy, you cannot `import def`, only
`abc.def`

Referring to a module symbol such as a function or const, only needs module name as prefix:

```v ignore
module def

// func is a dummy example function.
pub fn func() {
	println('func')
}
```

can be called like this:

```v ignore
module main

import def

fn main() {
	def.func()
}
```

A function, located in `abc/def/source.v`, is called with `def.func()`, not `abc.def.func()`

This always implies a *single prefix*, whatever sub-module depth. This behavior flattens
modules/sub-modules hierarchy. Should you have two modules with the same name in different
directories, then you should use Module import aliasing (see below).


### Module import aliasing

Any imported module name can be aliased using the `as` keyword:

> [!NOTE]
> This example will not compile unless you have created `mymod/sha256/somename.v`
> (submodule names are determined by their path, not by the names of the .v file(s) in them).

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
		year:  2020
		month: 12
		day:   25
	}
	println(time.new(my_time).utc_string())
	println('Century: ${my_time.century()}')
}
```

## Statements & expressions

### If

```v
a := 10
b := 20
if a < b {
	println('${a} < ${b}')
} else if a > b {
	println('${a} > ${b}')
} else {
	println('${a} == ${b}')
}
```

`if` statements are pretty straightforward and similar to most other languages.
Unlike other C-like languages,
there are no parentheses surrounding the condition and the braces are always required.

#### `If` expressions
Unlike C, V does not have a ternary operator, that would allow you to do: `x = c ? 1 : 2` .
Instead, it has a bit more verbose, but also clearer to read, ability to use `if` as an
expression. The direct translation in V of the ternary construct above, assuming `c` is a
boolean condition, would be: `x = if c { 1 } else { 2 }`.

Here is another example:
```v
num := 777
s := if num % 2 == 0 { 'even' } else { 'odd' }
println(s)
// "odd"
```

You can use multiple statements in each of the branches of an `if` expression, followed by a final
value, that will become the value of the entire `if` expression, when it takes that branch:
```v
n := arguments().len
x := if n > 2 {
	dump(arguments())
	42
} else {
	println('something else')
	100
}
dump(x)
```

#### `If` unwrapping
Anywhere you can use `or {}`, you can also use "if unwrapping". This binds the unwrapped value
of an expression to a variable when that expression is not none nor an error.

```v
m := {
	'foo': 'bar'
}

// handle missing keys
if v := m['foo'] {
	println(v) // bar
} else {
	println('not found')
}
```

```v
fn res() !int {
	return 42
}

// functions that return a result type
if v := res() {
	println(v)
}
```

```v
struct User {
	name string
}

arr := [User{'John'}]

// if unwrapping with assignment of a variable
u_name := if v := arr[0] {
	v.name
} else {
	'Unnamed'
}
println(u_name) // John
```

#### Type checks and casts

You can check the current type of a sum type using `is` and its negated form `!is`.

You can do it either in an `if`:

```v cgen
struct Abc {
	val string
}

struct Xyz {
	foo string
}

type Alphabet = Abc | Xyz

x := Alphabet(Abc{'test'}) // sum type
if x is Abc {
	// x is automatically cast to Abc and can be used here
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
		// x is automatically cast to Abc and can be used here
		println(x)
	}
	Xyz {
		// x is automatically cast to Xyz and can be used here
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
	// x.bar is automatically cast
	println(x.bar)
} else if x.bar is MyStruct2 {
	new_var := x.bar as MyStruct2
	// ... or you can use `as` to create a type cast an alias manually:
	println(new_var)
}
match x.bar {
	MyStruct {
		// x.bar is automatically cast
		println(x.bar)
	}
	else {}
}
```

Mutable variables can change, and doing a cast would be unsafe.
However, sometimes it's useful to type cast despite mutability.
In such cases the developer must mark the expression with the `mut` keyword
to tell the compiler that they know what they're doing.

It works like this:

```v oksyntax
mut x := MySumType(MyStruct{123})
if mut x is MyStruct {
	// x is cast to MyStruct even if it's mutable
	// without the mut keyword that wouldn't work
	println(x)
}
// same with match
match mut x {
	MyStruct {
		// x is cast to MyStruct even if it's mutable
		// without the mut keyword that wouldn't work
		println(x)
	}
}
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

A match statement can also to be used as an `if - else if - else` alternative:

```v
match true {
	2 > 4 { println('if') }
	3 == 4 { println('else if') }
	2 == 2 { println('else if2') }
	else { println('else') }
}
// 'else if2' should be printed
```

or as an `unless` alternative: [unless Ruby](https://www.tutorialspoint.com/ruby/ruby_if_else.htm)

```v
match false {
	2 > 4 { println('if') }
	3 == 4 { println('else if') }
	2 == 2 { println('else if2') }
	else { println('else') }
}
// 'if' should be printed
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

A match statement also can match the variant types of a `sumtype`. Note that
in that case, the match is exhaustive, since all variant types are mentioned
explicitly, so there is no need for an `else{}` branch.

```v nofmt
struct Dog {}
struct Cat {}
struct Veasel {}
type Animal = Dog | Cat | Veasel
a := Animal(Veasel{})
match a {
	Dog { println('Bay') }
	Cat { println('Meow') }
	Veasel { println('Vrrrrr-eeee') } // see: https://www.youtube.com/watch?v=qTJEDyj2N0Q
}
```

You can also use ranges as `match` patterns. If the value falls within the range
of a branch, that branch will be executed.

Note that the ranges use `...` (three dots) rather than `..` (two dots). This is
because the range is *inclusive* of the last element, rather than exclusive
(as `..` ranges are). Using `..` in a match branch will throw an error.

```v
const start = 1

const end = 10

c := 2
num := match c {
	start...end {
		1000
	}
	else {
		0
	}
}
println(num)
// 1000
```

Constants can also be used in the range branch expressions.

> [!NOTE]
> `match` as an expression is not usable in `for` loop and `if` statements.

### In operator

`in` allows to check whether an array or a map contains an element.
To do the opposite, use `!in`.

```v
nums := [1, 2, 3]
println(1 in nums) // true
println(4 !in nums) // true
```

> [!NOTE]
> `in` checks if map contains a key, not a value.

```v
m := {
	'one': 1
	'two': 2
}

println('one' in m) // true
println('three' !in m) // true
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

#### `for`/`in`

This is the most common form. You can use it with an array, map or
numeric range.

##### Array `for`

```v
numbers := [1, 2, 3, 4, 5]
for num in numbers {
	println(num)
}
names := ['Sam', 'Peter']
for i, name in names {
	println('${i}) ${name}')
	// Output: 0) Sam
	//         1) Peter
}
```

The `for value in arr` form is used for going through elements of an array.
If an index is required, an alternative form `for index, value in arr` can be used.

Note that the value is read-only.
If you need to modify the array while looping, you need to declare the element as mutable:

```v
mut numbers := [0, 1, 2]
for mut num in numbers {
	num++
}
println(numbers) // [1, 2, 3]
```

When an identifier is just a single underscore, it is ignored.

##### Custom iterators

Types that implement a `next` method returning an `Option` can be iterated
with a `for` loop.

```v
struct SquareIterator {
	arr []int
mut:
	idx int
}

fn (mut iter SquareIterator) next() ?int {
	if iter.idx >= iter.arr.len {
		return none
	}
	defer {
		iter.idx++
	}
	return iter.arr[iter.idx] * iter.arr[iter.idx]
}

nums := [1, 2, 3, 4, 5]
iter := SquareIterator{
	arr: nums
}
for squared in iter {
	println(squared)
}
```

The code above prints:

```
1
4
9
16
25
```

##### Map `for`

```v
m := {
	'one': 1
	'two': 2
}
for key, value in m {
	println('${key} -> ${value}')
	// Output: one -> 1
	//         two -> 2
}
```

Either key or value can be ignored by using a single underscore as the identifier.

```v
m := {
	'one': 1
	'two': 2
}
// iterate over keys
for key, _ in m {
	println(key)
	// Output: one
	//         two
}
// iterate over values
for _, value in m {
	println(value)
	// Output: 1
	//         2
}
```

##### Range `for`

```v
// Prints '01234'
for i in 0 .. 5 {
	print(i)
}
```

`low..high` means an *exclusive* range, which represents all values
from `low` up to *but not including* `high`.

> [!NOTE]
> This exclusive range notation and zero-based indexing follow principles of
logical consistency and error reduction. As Edsger W. Dijkstra outlines in
'Why Numbering Should Start at Zero'
([EWD831](https://www.cs.utexas.edu/users/EWD/transcriptions/EWD08xx/EWD831.html)),
zero-based indexing aligns the index with the preceding elements in a sequence,
simplifying handling and minimizing errors, especially with adjacent subsequences.
This logical and efficient approach shapes our language design, emphasizing clarity
and reducing confusion in programming.

#### Condition `for`

```v
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

```v
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

If the function returns a value the `defer` block is executed *after* the return
expression is evaluated:

```v
import os

enum State {
	normal
	write_log
	return_error
}

// write log file and return number of bytes written

fn write_log(s State) !int {
	mut f := os.create('log.txt')!
	defer {
		f.close()
	}
	if s == .write_log {
		// `f.close()` will be called after `f.write()` has been
		// executed, but before `write_log()` finally returns the
		// number of bytes written to `main()`
		return f.writeln('This is a log file')
	} else if s == .return_error {
		// the file will be closed after the `error()` function
		// has returned - so the error message will still report
		// it as open
		return error('nothing written; file open: ${f.is_opened}')
	}
	// the file will be closed here, too
	return 0
}

fn main() {
	n := write_log(.return_error) or {
		println('Error: ${err}')
		0
	}
	println('${n} bytes written')
}
```

To access the result of the function inside a `defer` block the `$res()` expression can be used.
`$res()` is only used when a single value is returned, while on multi-return the `$res(idx)`
is parameterized.

```v ignore
fn (mut app App) auth_middleware() bool {
	defer {
		if !$res() {
			app.response.status_code = 401
			app.response.body = 'Unauthorized'
		}
	}
	header := app.get_header('Authorization')
	if header == '' {
		return false
	}
	return true
}

fn (mut app App) auth_with_user_middleware() (bool, string) {
	defer {
		if !$res(0) {
			app.response.status_code = 401
			app.response.body = 'Unauthorized'
		} else {
			app.user = $res(1)
		}
	}
	header := app.get_header('Authorization')
	if header == '' {
		return false, ''
	}
	return true, 'TestUser'
}
```

### Goto

V allows unconditionally jumping to a label with `goto`. The label name must be contained
within the same function as the `goto` statement. A program may `goto` a label outside
or deeper than the current scope. `goto` allows jumping past variable initialization or
jumping back to code that accesses memory that has already been freed, so it requires
`unsafe`.

```v ignore
if x {
	// ...
	if y {
		unsafe {
			goto my_label
		}
	}
	// ...
}
my_label:
```

`goto` should be avoided, particularly when `for` can be used instead.
[Labelled break/continue](#labelled-break--continue) can be used to break out of
a nested loop, and those do not risk violating memory-safety.

## Structs

```v
struct Point {
	x int
	y int
}

mut p := Point{
	x: 10
	y: 20
}
println(p.x) // Struct fields are accessed using a dot
// Alternative literal syntax
p = Point{10, 20}
assert p.x == 10
```

Struct fields can re-use reserved keywords:

```v
struct Employee {
	type string
	name string
}

employee := Employee{
	type: 'FTE'
	name: 'John Doe'
}
println(employee.type)
```

### Heap structs

Structs are allocated on the stack. To allocate a struct on the heap
and get a [reference](#references) to it, use the `&` prefix:

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

```v
struct Foo {
mut:
	x int
}

fa := Foo{1}
mut a := fa
a.x = 2
assert fa.x == 1
assert a.x == 2

// fb := Foo{ 1 }
// mut b := &fb  // error: `fb` is immutable, cannot have a mutable reference to it
// b.x = 2

mut fc := Foo{1}
mut c := &fc
c.x = 2
assert fc.x == 2
assert c.x == 2
println(fc) // Foo{ x: 2 }
println(c) // &Foo{ x: 2 } // Note `&` prefixed.
```

see also [Stack and Heap](#stack-and-heap)

### Default field values

```v
struct Foo {
	n   int    // n is 0 by default
	s   string // s is '' by default
	a   []int  // a is `[]int{}` by default
	pos int = -1 // custom default value
}
```

All struct fields are zeroed by default during the creation of the struct.
Array and map fields are allocated.
In case of reference value, see [here](#structs-with-reference-fields).

It's also possible to define custom default values.

### Required fields

```v
struct Foo {
	n int @[required]
}
```

You can mark a struct field with the `[required]` [attribute](#attributes), to tell V that
that field must be initialized when creating an instance of that struct.

This example will not compile, since the field `n` isn't explicitly initialized:

```v failcompile
_ = Foo{}
```

<a id='short-struct-initialization-syntax'></a>

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
p = Point{
	x: 30
	y: 4
}
assert p.y == 4
//
// array: first element defines type of array
points := [Point{10, 20}, Point{20, 30}, Point{40, 50}]
println(points) // [Point{x: 10, y: 20}, Point{x: 20, y: 30}, Point{x: 40,y: 50}]
```

Omitting the struct name also works for returning a struct literal or passing one
as a function argument.

### Struct update syntax

V makes it easy to return a modified version of an object:

```v
struct User {
	name          string
	age           int
	is_registered bool
}

fn register(u User) User {
	return User{
		...u
		is_registered: true
	}
}

mut user := User{
	name: 'abc'
	age:  23
}
user = register(user)
println(user)
```

### Trailing struct literal arguments

V doesn't have default function arguments or named arguments, for that trailing struct
literal syntax can be used instead:

```v
@[params]
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
		width:  c.width
		height: c.height
		text:   c.text
	}
}

button := new_button(text: 'Click me', width: 100)
// the height is unset, so it's the default value
assert button.height == 20
```

As you can see, both the struct name and braces can be omitted, instead of:

```v oksyntax nofmt
new_button(ButtonConfig{text:'Click me', width:100})
```

This only works for functions that take a struct for the last argument.

> [!NOTE]
> Note the `[params]` tag is used to tell V, that the trailing struct parameter
> can be omitted *entirely*, so that you can write `button := new_button()`.
> Without it, you have to specify *at least* one of the field names, even if it
> has its default value, otherwise the compiler will produce this error message,
> when you call the function with no parameters:
> `error: expected 1 arguments, but got 0`.

### Access modifiers

Struct fields are private and immutable by default (making structs immutable as well).
Their access modifiers can be changed with
`pub` and `mut`. In total, there are 5 possible options:

```v
struct Foo {
	a int // private immutable (default)
mut:
	b int // private mutable
	c int // (you can list multiple fields with the same access modifier)
pub:
	d int // public immutable (readonly)
pub mut:
	e int // public, but mutable only in parent module
__global:
	// (not recommended to use, that's why the 'global' keyword starts with __)
	f int // public and mutable both inside and outside parent module
}
```

Private fields are available only inside the same [module](#modules), any attempt
to directly access them from another module will cause an error during compilation.
Public immutable fields are readonly everywhere.

### Anonymous structs

V supports anonymous structs: structs that don't have to be declared separately
with a struct name.

```v
struct Book {
	author struct {
		name string
		age  int
	}

	title string
}

book := Book{
	author: struct {
		name: 'Samantha Black'
		age:  24
	}
}
assert book.author.name == 'Samantha Black'
assert book.author.age == 24
```

### Static type methods

V now supports static type methods like `User.new()`. These are defined on a struct via
`fn [Type name].[function name]` and allow to organize all functions related to a struct:

```v oksyntax
struct User {}

fn User.new() User {
	return User{}
}

user := User.new()
```

This is an alternative to factory functions like `fn new_user() User {}` and should be used
instead.

> [!NOTE]
> Note, that these are not constructors, but simple functions. V doesn't have constructors or
> classes.

### `[noinit]` structs

V supports `[noinit]` structs, which are structs that cannot be initialised outside the module
they are defined in. They are either meant to be used internally or they can be used externally
through _factory functions_.

For an example, consider the following source in a directory `sample`:

```v oksyntax
module sample

@[noinit]
pub struct Information {
pub:
	data string
}

pub fn new_information(data string) !Information {
	if data.len == 0 || data.len > 100 {
		return error('data must be between 1 and 100 characters')
	}
	return Information{
		data: data
	}
}
```

Note that `new_information` is a _factory_ function. Now when we want to use this struct
outside the module:

```v okfmt
import sample

fn main() {
	// This doesn't work when the [noinit] attribute is present:
	// info := sample.Information{
	// 	data: 'Sample information.'
	// }

	// Use this instead:
	info := sample.new_information('Sample information.')!

	println(info)
}
```

### Methods

```v
struct User {
	age int
}

fn (u User) can_register() bool {
	return u.age > 16
}

user := User{
	age: 10
}
println(user.can_register()) // "false"
user2 := User{
	age: 20
}
println(user2.can_register()) // "true"
```

V doesn't have classes, but you can define methods on types.
A method is a function with a special receiver argument.
The receiver appears in its own argument list between the `fn` keyword and the method name.
Methods must be in the same module as the receiver type.

In this example, the `can_register` method has a receiver of type `User` named `u`.
The convention is not to use receiver names like `self` or `this`,
but a short, preferably one letter long, name.

### Embedded structs

V supports embedded structs.

```v
struct Size {
mut:
	width  int
	height int
}

fn (s &Size) area() int {
	return s.width * s.height
}

struct Button {
	Size
	title string
}
```

With embedding, the struct `Button` will automatically get all the fields and methods from
the struct `Size`, which allows you to do:

```v oksyntax
mut button := Button{
	title:  'Click me'
	height: 2
}

button.width = 3
assert button.area() == 6
assert button.Size.area() == 6
print(button)
```

output :

```
Button{
    Size: Size{
        width: 3
        height: 2
    }
    title: 'Click me'
}
```

Unlike inheritance, you cannot type cast between structs and embedded structs
(the embedding struct can also have its own fields, and it can also embed multiple structs).

If you need to access embedded structs directly, use an explicit reference like `button.Size`.

Conceptually, embedded structs are similar to [mixin](https://en.wikipedia.org/wiki/Mixin)s
in OOP, *NOT* base classes.

You can also initialize an embedded struct:

```v oksyntax
mut button := Button{
	Size: Size{
		width:  3
		height: 2
	}
}
```

or assign values:

```v oksyntax
button.Size = Size{
	width:  4
	height: 5
}
```

If multiple embedded structs have methods or fields with the same name, or if methods or fields
with the same name are defined in the struct, you can call methods or assign to variables in
the embedded struct like `button.Size.area()`.
When you do not specify the embedded struct name, the method of the outermost struct will be
targeted.

## Unions

A union is a special type of struct that allows storing different data types in the same memory
location. You can define a union with many members, but only one member may contain a valid value
at any time, depending on the data types of the members. Unions provide an efficient way of using
the same memory location for multiple purposes.

All the members of a union share the same memory location. This means that modifying one member
automatically modifies all the rest. The largest union member defines the size of the union.

### Why use unions?

One reason, as stated above, is to use less memory for storing things.  Since the size of a union
is the size of the largest field in it, and the size of a struct is the size of all the fields in
the struct added together, a union is definitely better for lower memory usage.  As long as you
only need one of the fields to be valid at any time, the union wins.

Another reason is to allow easier access to parts of a field.  For example, without using a union,
if you want to look at each of the bytes of a 32-bit integer separately, you'll need bitwise
`RIGHT-SHIFT`s and `AND` operations.  With a union, you can access the individual bytes directly.
```v
union ThirtyTwo {
	a u32
	b [4]u8
}
```
Since `ThirtyTwo.a` and `ThirtyTwo.b` share the same memory locations, you can directly access
each byte of `a` by referencing `b[byte_offset]`.

### Embedding

Unions also support embedding, the same as structs.

```v
struct Rgba32_Component {
	r u8
	g u8
	b u8
	a u8
}

union Rgba32 {
	Rgba32_Component
	value u32
}

clr1 := Rgba32{
	value: 0x008811FF
}

clr2 := Rgba32{
	Rgba32_Component: Rgba32_Component{
		a: 128
	}
}

sz := sizeof(Rgba32)
unsafe {
	println('Size: ${sz}B,clr1.b: ${clr1.b},clr2.b: ${clr2.b}')
}
```

Output: `Size: 4B, clr1.b: 136, clr2.b: 0`

Union member access must be performed in an `unsafe` block.

> [!NOTE]
> Embedded struct arguments are not necessarily stored in the order listed.

## Functions 2

### Immutable function args by default

In V function arguments are immutable by default, and mutable args have to be
marked on call.

Since there are also no globals, that means that the return values of the functions,
are a function of their arguments only, and their evaluation has no side effects
(unless the function uses I/O).

Function arguments are immutable by default, even when [references](#references) are passed.

> [!NOTE]
> However, V is not a purely functional language.

There is a compiler flag to enable global variables (`-enable-globals`), but this is
intended for low-level applications like kernels and drivers.

### Mutable arguments

It is possible to modify function arguments by declaring them with the keyword `mut`:

```v
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

In this example, the receiver (which is just the first argument) is explicitly marked as mutable,
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

Note that you have to add `mut` before `nums` when calling this function. This makes
it clear that the function being called will modify the value.

It is preferable to return values instead of modifying arguments,
e.g. `user = register(user)` (or `user.register()`) instead of `register(mut user)`.
Modifying arguments should only be done in performance-critical parts of your application
to reduce allocations and copying.

For this reason V doesn't allow the modification of arguments with primitive types (e.g. integers).
Only more complex types such as arrays and maps may be modified.

### Variable number of arguments
V supports functions that receive an arbitrary, variable amounts of arguments, denoted with the
`...` prefix.
Below, `a ...int` refers to an arbitrary amount of parameters that will be collected
into an array named `a`.

```v
fn sum(a ...int) int {
	mut total := 0
	for x in a {
		total += x
	}
	return total
}

println(sum()) // 0
println(sum(1)) // 1
println(sum(2, 3)) // 5
// using array decomposition
a := [2, 3, 4]
println(sum(...a)) // <-- using prefix ... here. output: 9
b := [5, 6, 7]
println(sum(...b)) // output: 18
```

### Anonymous & higher order functions

```v
fn sqr(n int) int {
	return n * n
}

fn cube(n int) int {
	return n * n * n
}

fn run(value int, op fn (int) int) int {
	return op(value)
}

fn main() {
	// Functions can be passed to other functions
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
	println(res) // "10"
	// You can even have an array/map of functions:
	fns := [sqr, cube]
	println(fns[0](10)) // "100"
	fns_map := {
		'sqr':  sqr
		'cube': cube
	}
	println(fns_map['cube'](2)) // "8"
}
```

### Lambda expressions

Lambda expressions in V are small anonymous functions, defined using
the `|variables| expression` syntax. Note: this syntax is valid only inside calls to higher
order functions.

Here are some examples:
```v
mut a := [1, 2, 3]
a.sort(|x, y| x > y) // sorts the array, defining the comparator with a lambda expression
println(a.map(|x| x * 10)) // prints [30, 20, 10]
```

```v
// Lambda function can be used as callback
fn f(cb fn (a int) int) int {
	return cb(10)
}

println(f(|x| x + 4)) // prints 14
```

### Closures

V supports closures too.
This means that anonymous functions can inherit variables from the scope they were created in.
They must do so explicitly by listing all variables that are inherited.

```v oksyntax
my_int := 1
my_closure := fn [my_int] () {
	println(my_int)
}
my_closure() // prints 1
```

Inherited variables are copied when the anonymous function is created.
This means that if the original variable is modified after the creation of the function,
the modification won't be reflected in the function.

```v oksyntax
mut i := 1
func := fn [i] () int {
	return i
}
println(func() == 1) // true
i = 123
println(func() == 1) // still true
```

However, the variable can be modified inside the anonymous function.
The change won't be reflected outside, but will be in the later function calls.

```v oksyntax
fn new_counter() fn () int {
	mut i := 0
	return fn [mut i] () int {
		i++
		return i
	}
}

c := new_counter()
println(c()) // 1
println(c()) // 2
println(c()) // 3
```

If you need the value to be modified outside the function, use a reference.

```v oksyntax
mut i := 0
mut ref := &i
print_counter := fn [ref] () {
	println(*ref)
}

print_counter() // 0
i = 10
print_counter() // 10
```

### Parameter evaluation order

The evaluation order of the parameters of function calls is *NOT* guaranteed.
Take for example the following program:

```v
fn f(a1 int, a2 int, a3 int) {
	dump(a1 + a2 + a3)
}

fn main() {
	f(dump(100), dump(200), dump(300))
}
```

V currently does not guarantee that it will print 100, 200, 300 in that order.
The only guarantee is that 600 (from the body of `f`) will be printed after all of them.

This *may* change in V 1.0 .

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

```v
struct Node[T] {
	val   T
	left  &Node[T]
	right &Node[T]
}
```

To dereference a reference, use the `*` operator, just like in C.

## Constants

```v
const pi = 3.14
const world = '世界'

println(pi)
println(world)
```

Constants are declared with `const`. They can only be defined
at the module level (outside of functions).
Constant values can never be changed. You can also declare a single
constant separately:

```v
const e = 2.71828
```

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

const numbers = [1, 2, 3]
const red = Color{
	r: 255
	g: 0
	b: 0
}
// evaluate function call at compile time*
const blue = rgb(0, 0, 255)

println(numbers)
println(red)
println(blue)
```

\* WIP - for now function calls are evaluated at program start-up

Global variables are not normally allowed, so this can be really useful.

**Modules**

Constants can be made public with `pub const`:

```v oksyntax
module mymodule

pub const golden_ratio = 1.61803

fn calc() {
	println(golden_ratio)
}
```

The `pub` keyword is only allowed before the `const` keyword and cannot be used inside
a `const ( )` block.

Outside from module main all constants need to be prefixed with the module name.

### Required module prefix

When naming constants, `snake_case` must be used. In order to distinguish consts
from local variables, the full path to consts must be specified. For example,
to access the PI const, full `math.pi` name must be used both outside the `math`
module, and inside it. That restriction is relaxed only for the `main` module
(the one containing your `fn main()`), where you can use the unqualified name of
constants defined there, i.e. `numbers`, rather than `main.numbers`.

vfmt takes care of this rule, so you can type `println(pi)` inside the `math` module,
and vfmt will automatically update it to `println(math.pi)`.

<!--
Many people prefer all caps consts: `TOP_CITIES`. This wouldn't work
well in V, because consts are a lot more powerful than in other languages.
They can represent complex structures, and this is used quite often since there
are no globals:

```v oksyntax
println('Top cities: ${top_cities.filter(.usa)}')
```
-->

## Builtin functions

Some functions are builtin like `println`. Here is the complete list:

```v ignore
fn print(s string) // prints anything on stdout
fn println(s string) // prints anything and a newline on stdout

fn eprint(s string) // same as print(), but uses stderr
fn eprintln(s string) // same as println(), but uses stderr

fn exit(code int) // terminates the program with a custom error code
fn panic(s string) // prints a message and backtraces on stderr, and terminates the program with error code 1
fn print_backtrace() // prints backtraces on stderr
```

> [!NOTE]
> Although the `print` functions take a string, V accepts other printable types too.
> See below for details.

There is also a special built-in function called [`dump`](#dumping-expressions-at-runtime).

### println

`println` is a simple yet powerful builtin function, that can print anything:
strings, numbers, arrays, maps, structs.

```v
struct User {
	name string
	age  int
}

println(1) // "1"
println('hi') // "hi"
println([1, 2, 3]) // "[1, 2, 3]"
println(User{ name: 'Bob', age: 20 }) // "User{name:'Bob', age:20}"
```

See also [String interpolation](#string-interpolation).

<a id='custom-print-of-types'></a>

### Printing custom types

If you want to define a custom print value for your type, simply define a
`str() string` method:

```v
struct Color {
	r int
	g int
	b int
}

pub fn (c Color) str() string {
	return '{${c.r}, ${c.g}, ${c.b}}'
}

red := Color{
	r: 255
	g: 0
	b: 0
}
println(red)
```

### Dumping expressions at runtime

You can dump/trace the value of any V expression using `dump(expr)`.
For example, save this code sample as `factorial.v`, then run it with
`v run factorial.v`:

```v
fn factorial(n u32) u32 {
	if dump(n <= 1) {
		return dump(1)
	}
	return dump(n * factorial(n - 1))
}

fn main() {
	println(factorial(5))
}
```

You will get:

```
[factorial.v:2] n <= 1: false
[factorial.v:2] n <= 1: false
[factorial.v:2] n <= 1: false
[factorial.v:2] n <= 1: false
[factorial.v:2] n <= 1: true
[factorial.v:3] 1: 1
[factorial.v:5] n * factorial(n - 1): 2
[factorial.v:5] n * factorial(n - 1): 6
[factorial.v:5] n * factorial(n - 1): 24
[factorial.v:5] n * factorial(n - 1): 120
120
```

Note that `dump(expr)` will trace both the source location,
the expression itself, and the expression value.

## Modules

Every file in the root of a folder is part of the same module.
Simple programs don't need to specify module name, in which case it defaults to 'main'.

See [symbol visibility](#symbol-visibility), [Access modifiers](#access-modifiers).

### Create modules

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
All items inside a module can be used between the files of a module regardless of whether or
not they are prefaced with the `pub` keyword.
```v failcompile
// myfile2.v
module mymodule

pub fn say_hi_and_bye() {
	say_hi() // from myfile.v
	println('goodbye from mymodule')
}
```

You can now use `mymodule` in your code:

```v failcompile
import mymodule

fn main() {
	mymodule.say_hi()
	mymodule.say_hi_and_bye()
}
```

* Module names should be short, under 10 characters.
* Module names must use `snake_case`.
* Circular imports are not allowed.
* You can have as many .v files in a module as you want.
* You can create modules anywhere.
* All modules are compiled statically into a single executable.

### Special considerations for project folders

For the top level project folder (the one, compiled with `v .`), and *only*
that folder, you can have several .v files, that may be mentioning different modules
with `module main`, `module abc` etc

This is to ease the prototyping workflow in that folder:
- you can start developing some new project with a single .v file
- split functionality as necessary to different .v files in the same folder
- when that makes logical sense to be further organised, put them into their own directory module.

Note that in ordinary modules, all .v files must start with `module name_of_folder`.

### `init` functions

If you want a module to automatically call some setup/initialization code when it is imported,
you can define a module `init` function:

```v
fn init() {
	// your setup code here ...
}
```

The `init` function cannot be public - it will be called automatically by V, *just once*, no matter
how many times the module was imported in your program. This feature is particularly useful for
initializing a C library.

### `cleanup` functions

If you want a module to automatically call some cleanup/deinitialization code, when your program
ends, you can define a module `cleanup` function:

```v
fn cleanup() {
	// your deinitialisation code here ...
}
```

Just like the `init` function, the `cleanup` function for a module cannot be public - it will be
called automatically, when your program ends, once per module, even if the module was imported
transitively by other modules several times, in the reverse order of the init calls.

## Type Declarations

### Type aliases

To define a new type `NewType` as an alias for `ExistingType`,
do `type NewType = ExistingType`.<br/>
This is a special case of a [sum type](#sum-types) declaration.

### Enums

An enum is a group of constant integer values, each having its own name,
whose values start at 0 and increase by 1 for each name listed.
For example:
```v
enum Color as u8 {
	red   // the default start value is 0
	green // the value is automatically incremented to 1
	blue  // the final value is now 2
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
println(int(color)) // prints 1
```

The enum type can be any integer type, but can be omitted, if it is `int`: `enum Color {`.

Enum match must be exhaustive or have an `else` branch.
This ensures that if a new enum field is added, it's handled everywhere in the code.

Enum fields can re-use reserved keywords:

```v
enum Color {
	none
	red
	green
	blue
}

color := Color.none
println(color)
```

Integers may be assigned to enum fields.

```v
enum Grocery {
	apple
	orange = 5
	pear
}

g1 := int(Grocery.apple)
g2 := int(Grocery.orange)
g3 := int(Grocery.pear)
println('Grocery IDs: ${g1}, ${g2}, ${g3}')
```

Output: `Grocery IDs: 0, 5, 6`.

Operations are not allowed on enum variables; they must be explicitly cast to `int`.

Enums can have methods, just like structs.

```v
enum Cycle {
	one
	two
	three
}

fn (c Cycle) next() Cycle {
	match c {
		.one {
			return .two
		}
		.two {
			return .three
		}
		.three {
			return .one
		}
	}
}

mut c := Cycle.one
for _ in 0 .. 10 {
	println(c)
	c = c.next()
}
```

Output:

```
one
two
three
one
two
three
one
two
three
one
```

Enums can be created from string or integer value and converted into string

```v
enum Cycle {
	one
	two = 2
	three
}

// Create enum from value
println(Cycle.from(10) or { Cycle.three })
println(Cycle.from('two')!)

// Convert an enum value to a string
println(Cycle.one.str())
```

Output:

```
three
two
one
```

### Function Types

You can use type aliases for naming specific function signatures - for
example:

```v
type Filter = fn (string) string
```

This works like any other type - for example, a function can accept an
argument of a function type:

```v
type Filter = fn (string) string

fn filter(s string, f Filter) string {
	return f(s)
}
```

V has duck-typing, so functions don't need to declare compatibility with
a function type - they just have to be compatible:

```v
fn uppercase(s string) string {
	return s.to_upper()
}

// now `uppercase` can be used everywhere where Filter is expected
```

Compatible functions can also be explicitly cast to a function type:

```v oksyntax
my_filter := Filter(uppercase)
```

The cast here is purely informational - again, duck-typing means that the
resulting type is the same without an explicit cast:

```v oksyntax
my_filter := uppercase
```

You can pass the assigned function as an argument:

```v oksyntax
println(filter('Hello world', my_filter)) // prints `HELLO WORLD`
```

And you could of course have passed it directly as well, without using a
local variable:

```v oksyntax
println(filter('Hello world', uppercase))
```

And this works with anonymous functions as well:

```v oksyntax
println(filter('Hello world', fn (s string) string {
	return s.to_upper()
}))
```

You can see the complete
[example here](https://github.com/vlang/v/tree/master/examples/function_types.v).

### Interfaces

```v
// interface-example.1
struct Dog {
	breed string
}

fn (d Dog) speak() string {
	return 'woof'
}

struct Cat {
	breed string
}

fn (c Cat) speak() string {
	return 'meow'
}

// unlike Go, but like TypeScript, V's interfaces can define both fields and methods.
interface Speaker {
	breed string
	speak() string
}

fn main() {
	dog := Dog{'Leonberger'}
	cat := Cat{'Siamese'}

	mut arr := []Speaker{}
	arr << dog
	arr << cat
	for item in arr {
		println('a ${item.breed} says: ${item.speak()}')
	}
}
```

#### Implement an interface

A type implements an interface by implementing its methods and fields.

An interface can have a `mut:` section. Implementing types will need
to have a `mut` receiver, for methods declared in the `mut:` section
of an interface.

```v
// interface-example.2
module main

interface Foo {
	write(string) string
}

// => the method signature of a type, implementing interface Foo should be:
// `fn (s Type) write(a string) string`

interface Bar {
mut:
	write(string) string
}

// => the method signature of a type, implementing interface Bar should be:
// `fn (mut s Type) write(a string) string`

struct MyStruct {}

// MyStruct implements the interface Foo, but *not* interface Bar
fn (s MyStruct) write(a string) string {
	return a
}

fn main() {
	s1 := MyStruct{}
	fn1(s1)
	// fn2(s1) -> compile error, since MyStruct does not implement Bar
}

fn fn1(s Foo) {
	println(s.write('Foo'))
}

// fn fn2(s Bar) { // does not match
//      println(s.write('Foo'))
// }
```

There is an **optional** `implements` keyword for explicit declaration
of intent, which applies to `struct` declarations.

```v
struct PathError implements IError {
	Error
	path string
}

fn (err PathError) msg() string {
	return 'Failed to open path: ${err.path}'
}

fn try_open(path string) ! {
	return PathError{
		path: path
	}
}

fn main() {
	try_open('/tmp') or { panic(err) }
}
```

#### Casting an interface

We can test the underlying type of an interface using dynamic cast operators.
> [!NOTE]
> Dynamic cast converts variable `s` into a pointer inside the `if` statements in this example:

```v oksyntax
// interface-example.3 (continued from interface-example.1)
interface Something {}

fn announce(s Something) {
	if s is Dog {
		println('a ${s.breed} dog') // `s` is automatically cast to `Dog` (smart cast)
	} else if s is Cat {
		println('a cat speaks ${s.speak()}')
	} else {
		println('something else')
	}
}

fn main() {
	dog := Dog{'Leonberger'}
	cat := Cat{'Siamese'}
	announce(dog)
	announce(cat)
}
```

```v
// interface-example.4
interface IFoo {
	foo()
}

interface IBar {
	bar()
}

// implements only IFoo
struct SFoo {}

fn (sf SFoo) foo() {}

// implements both IFoo and IBar
struct SFooBar {}

fn (sfb SFooBar) foo() {}

fn (sfb SFooBar) bar() {
	dump('This implements IBar')
}

fn main() {
	mut arr := []IFoo{}
	arr << SFoo{}
	arr << SFooBar{}

	for a in arr {
		dump(a)
		// In order to execute instances that implements IBar.
		if a is IBar {
			a.bar()
		}
	}
}
```

For more information, see [Dynamic casts](#dynamic-casts).

#### Interface method definitions

Also unlike Go, an interface can have its own methods, similar to how
structs can have their methods. These 'interface methods' do not have
to be implemented, by structs which implement that interface.
They are just a convenient way to write `i.some_function()` instead of
`some_function(i)`, similar to how struct methods can be looked at, as
a convenience for writing `s.xyz()` instead of `xyz(s)`.

> [!NOTE]
> This feature is NOT a "default implementation" like in C#.

For example, if a struct `cat` is wrapped in an interface `a`, that has
implemented a method with the same name `speak`, as a method implemented by
the struct, and you do `a.speak()`, *only* the interface method is called:

```v
interface Adoptable {}

fn (a Adoptable) speak() string {
	return 'adopt me!'
}

struct Cat {}

fn (c Cat) speak() string {
	return 'meow!'
}

struct Dog {}

fn main() {
	cat := Cat{}
	assert dump(cat.speak()) == 'meow!'

	a := Adoptable(cat)
	assert dump(a.speak()) == 'adopt me!' // call Adoptable's `speak`
	if a is Cat {
		// Inside this `if` however, V knows that `a` is not just any
		// kind of Adoptable, but actually a Cat, so it will use the
		// Cat `speak`, NOT the Adoptable `speak`:
		dump(a.speak()) // meow!
	}

	b := Adoptable(Dog{})
	assert dump(b.speak()) == 'adopt me!' // call Adoptable's `speak`
	// if b is Dog {
	// 	dump(b.speak()) // error: unknown method or field: Dog.speak
	// }
}
```

#### Embedded interface

Interfaces support embedding, just like structs:

```v
pub interface Reader {
mut:
	read(mut buf []u8) ?int
}

pub interface Writer {
mut:
	write(buf []u8) ?int
}

// ReaderWriter embeds both Reader and Writer.
// The effect is the same as copy/pasting all of the
// Reader and all of the Writer methods/fields into
// ReaderWriter.
pub interface ReaderWriter {
	Reader
	Writer
}
```

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

With sum types you could build recursive structures and write concise but powerful code on them.

```v
// V's binary tree
struct Empty {}

struct Node {
	value f64
	left  Tree
	right Tree
}

type Tree = Empty | Node

// sum up all node values

fn sum(tree Tree) f64 {
	return match tree {
		Empty { 0 }
		Node { tree.value + sum(tree.left) + sum(tree.right) }
	}
}

fn main() {
	left := Node{0.2, Empty{}, Empty{}}
	right := Node{0.3, Empty{}, Node{0.4, Empty{}, Empty{}}}
	tree := Node{0.5, left, right}
	println(sum(tree)) // 0.2 + 0.3 + 0.4 + 0.5 = 1.4
}
```

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
known as *flow-sensitive typing*.
If `w` is a mutable identifier, it would be unsafe if the compiler smart casts it without a warning.
That's why you have to declare a `mut` before the `is` expression:

```v ignore
if mut w is Mars {
	assert typeof(w).name == 'Mars'
	if w.dust_storm() {
		println('bad weather!')
	}
}
```

Otherwise `w` would keep its original type.
> This works for both simple variables and complex expressions like `user.name`

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
}
```

### Option/Result types and error handling

Option types are for types which may represent `none`. Result types may
represent an error returned from a function.

`Option` types are declared by prepending `?` to the type name: `?Type`.
`Result` types use `!`: `!Type`.

```v
struct User {
	id   int
	name string
}

struct Repo {
	users []User
}

fn (r Repo) find_user_by_id(id int) !User {
	for user in r.users {
		if user.id == id {
			// V automatically wraps this into a result or option type
			return user
		}
	}
	return error('User ${id} not found')
}

// A version of the function using an option
fn (r Repo) find_user_by_id2(id int) ?User {
	for user in r.users {
		if user.id == id {
			return user
		}
	}
	return none
}

fn main() {
	repo := Repo{
		users: [User{1, 'Andrew'}, User{2, 'Bob'}, User{10, 'Charles'}]
	}
	user := repo.find_user_by_id(10) or { // Option/Result types must be handled by `or` blocks
		println(err)
		return
	}
	println(user.id) // "10"
	println(user.name) // "Charles"

	user2 := repo.find_user_by_id2(10) or { return }

	// To create an Option var directly:
	my_optional_int := ?int(none)
	my_optional_string := ?string(none)
	my_optional_user := ?User(none)
}
```

V used to combine `Option` and `Result` into one type, now they are separate.

The amount of work required to "upgrade" a function to an option/result function is minimal;
you have to add a `?` or `!` to the return type and return `none` or an error (respectively)
when something goes wrong.

This is the primary mechanism for error handling in V. They are still values, like in Go,
but the advantage is that errors can't be unhandled, and handling them is a lot less verbose.
Unlike other languages, V does not handle exceptions with `throw/try/catch` blocks.

`err` is defined inside an `or` block and is set to the string message passed
to the `error()` function.

```v oksyntax
user := repo.find_user_by_id(7) or {
	println(err) // "User 7 not found"
	return
}
```

Use `err is ...` to compare errors:
```v oksyntax
import io

x := read() or {
	if err is io.Eof {
		println('end of file')
	}
	return
}
```

#### Options/results when returning multiple values

Only one `Option` or `Result` is allowed to be returned from a function. It is
possible to return multiple values and still signal an error.

```v
fn multi_return(v int) !(int, int) {
	if v < 0 {
		return error('must be positive')
	}
	return v, v * v
}
```

#### Handling options/results

There are four ways of handling an option/result. The first method is to
propagate the error:

```v
import net.http

fn f(url string) !string {
	resp := http.get(url)!
	return resp.body
}
```

`http.get` returns `!http.Response`. Because `!` follows the call, the
error will be propagated to the caller of `f`. When using `?` after a
function call producing an option, the enclosing function must return
an option as well. If error propagation is used in the `main()`
function it will `panic` instead, since the error cannot be propagated
any further.

The body of `f` is essentially a condensed version of:

```v ignore
    resp := http.get(url) or { return err }
    return resp.body
```

---
The second method is to break from execution early:

```v oksyntax
user := repo.find_user_by_id(7) or { return }
```

Here, you can either call `panic()` or `exit()`, which will stop the execution of the
entire program, or use a control flow statement (`return`, `break`, `continue`, etc)
to break from the current block.

> [!NOTE]
> `break` and `continue` can only be used inside a `for` loop.

V does not have a way to forcibly "unwrap" an option (as other languages do,
for instance Rust's `unwrap()` or Swift's `!`). To do this, use `or { panic(err) }` instead.

---
The third method is to provide a default value at the end of the `or` block.
In case of an error, that value would be assigned instead,
so it must have the same type as the content of the `Option` being handled.

```v
fn do_something(s string) !string {
	if s == 'foo' {
		return 'foo'
	}
	return error('invalid string')
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
	println(resp.body) // resp is a http.Response, not an option
} else {
	println(err)
}
```

Above, `http.get` returns a `!http.Response`. `resp` is only in scope for the first
`if` branch. `err` is only in scope for the `else` branch.

### Custom error types

V gives you the ability to define custom error types through the `IError` interface.
The interface requires two methods: `msg() string` and `code() int`. Every type that
implements these methods can be used as an error.

When defining a custom error type it is recommended to embed the builtin `Error` default
implementation. This provides an empty default implementation for both required methods,
so you only have to implement what you really need, and may provide additional utility
functions in the future.

```v
struct PathError {
	Error
	path string
}

fn (err PathError) msg() string {
	return 'Failed to open path: ${err.path}'
}

fn try_open(path string) ! {
	// V automatically casts this to IError
	return PathError{
		path: path
	}
}

fn main() {
	try_open('/tmp') or { panic(err) }
}
```

### Generics

```v wip

struct Repo[T] {
    db DB
}

struct User {
	id   int
	name string
}

struct Post {
	id   int
	user_id int
	title string
	body string
}

fn new_repo[T](db DB) Repo[T] {
    return Repo[T]{db: db}
}

// This is a generic function. V will generate it for every type it's used with.
fn (r Repo[T]) find_by_id(id int) ?T {
    table_name := T.name // in this example getting the name of the type gives us the table name
    return r.db.query_one[T]('select * from ${table_name} where id = ?', id)
}

db := new_db()
users_repo := new_repo[User](db) // returns Repo[User]
posts_repo := new_repo[Post](db) // returns Repo[Post]
user := users_repo.find_by_id(1)? // find_by_id[User]
post := posts_repo.find_by_id(1)? // find_by_id[Post]
```

Currently generic function definitions must declare their type parameters, but in
future V will infer generic type parameters from single-letter type names in
runtime parameter types. This is why `find_by_id` can omit `[T]`, because the
receiver argument `r` uses a generic type `T`.

Another example:

```v
fn compare[T](a T, b T) int {
	if a < b {
		return -1
	}
	if a > b {
		return 1
	}
	return 0
}

// compare[int]
println(compare(1, 0)) // Outputs: 1
println(compare(1, 1)) //          0
println(compare(1, 2)) //         -1
// compare[string]
println(compare('1', '0')) // Outputs: 1
println(compare('1', '1')) //          0
println(compare('1', '2')) //         -1
// compare[f64]
println(compare(1.1, 1.0)) // Outputs: 1
println(compare(1.1, 1.1)) //          0
println(compare(1.1, 1.2)) //         -1
```

## Concurrency

### Spawning Concurrent Tasks

V's model of concurrency is similar to Go's.

`go foo()` runs `foo()` concurrently in a lightweight thread managed by the V runtime.

`spawn foo()` runs `foo()` concurrently in a different thread:

```v
import math

fn p(a f64, b f64) { // ordinary function without return value
	c := math.sqrt(a * a + b * b)
	println(c)
}

fn main() {
	spawn p(3, 4)
	// p will be run in parallel thread
	// It can also be written as follows
	// spawn fn (a f64, b f64) {
	// 	c := math.sqrt(a * a + b * b)
	// 	println(c)
	// }(3, 4)
}
```

> [!NOTE]
> Threads rely on the machine's CPU (number of cores/threads).
> Be aware that OS threads spawned with `spawn`
> have limitations in regard to concurrency,
> including resource overhead and scalability issues,
> and might affect performance in cases of high thread count.

Sometimes it is necessary to wait until a parallel thread has finished. This can
be done by assigning a *handle* to the started thread and calling the `wait()` method
to this handle later:

```v
import math

fn p(a f64, b f64) { // ordinary function without return value
	c := math.sqrt(a * a + b * b)
	println(c) // prints `5`
}

fn main() {
	h := spawn p(3, 4)
	// p() runs in parallel thread
	h.wait()
	// p() has definitely finished
}
```

This approach can also be used to get a return value from a function that is run in a
parallel thread. There is no need to modify the function itself to be able to call it
concurrently.

```v
import math { sqrt }

fn get_hypot(a f64, b f64) f64 { //       ordinary function returning a value
	c := sqrt(a * a + b * b)
	return c
}

fn main() {
	g := spawn get_hypot(54.06, 2.08) // spawn thread and get handle to it
	h1 := get_hypot(2.32, 16.74) //   do some other calculation here
	h2 := g.wait() //                 get result from spawned thread
	println('Results: ${h1}, ${h2}') //   prints `Results: 16.9, 54.1`
}
```

If there is a large number of tasks, it might be easier to manage them
using an array of threads.

```v
import time

fn task(id int, duration int) {
	println('task ${id} begin')
	time.sleep(duration * time.millisecond)
	println('task ${id} end')
}

fn main() {
	mut threads := []thread{}
	threads << spawn task(1, 500)
	threads << spawn task(2, 900)
	threads << spawn task(3, 100)
	threads.wait()
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

Additionally for threads that return the same type, calling `wait()`
on the thread array will return all computed values.

```v
fn expensive_computing(i int) int {
	return i * i
}

fn main() {
	mut threads := []thread int{}
	for i in 1 .. 10 {
		threads << spawn expensive_computing(i)
	}
	// Join all tasks
	r := threads.wait()
	println('All jobs finished: ${r}')
}

// Output: All jobs finished: [1, 4, 9, 16, 25, 36, 49, 64, 81]
```

### Channels

Channels are the preferred way to communicate between threads. They allow threads to exchange data
safely without requiring explicit locking. V's channels are similar to those in Go, enabling you
to push objects into a channel on one end and pop objects from the other.
Channels can be buffered or unbuffered, and you can use the `select` statement to monitor multiple
channels simultaneously.

#### Syntax and Usage

Channels are declared with the type `chan objtype`.
You can optionally specify a buffer length using the `cap` field:

```v
ch := chan int{} // unbuffered - "synchronous"
ch2 := chan f64{cap: 100} // buffered with a capacity of 100
```

Channels do not have to be declared as `mut`. The buffer length is not part of the type but
a field of the individual channel object. Channels can be passed to threads like normal
variables:

```v
import time

fn worker(ch chan int) {
	for i in 0 .. 5 {
		ch <- i // push values into the channel
	}
}

fn clock(ch chan int) {
	for i in 0 .. 5 {
		time.sleep(1 * time.second)
		println('Clock tick')
		ch <- (i + 1000) // push a value into the channel
	}
	ch.close() // close the channel when done
}

fn main() {
	ch := chan int{cap: 5}
	spawn worker(ch)
	spawn clock(ch)
	for {
		value := <-ch or { // receive/pop values from the channel
			println('Channel closed')
			break
		}
		println('Received: ${value}')
	}
}
```

#### Buffered Channels

Buffered channels allow you to push multiple items without blocking,
as long as the buffer is not full:

```v
ch := chan string{cap: 2}
ch <- 'hello'
ch <- 'world'
// ch <- '!' // This would block because the buffer is full

println(<-ch) // "hello"
println(<-ch) // "world"
```

#### Closing Channels

A channel can be closed to indicate that no further objects can be pushed. Any attempt
to do so will then result in a runtime panic (with the exception of `select` and
`try_push()` - see below). Attempts to pop will return immediately if the
associated channel has been closed and the buffer is empty. This situation can be
handled using an `or {}` block (see [Handling options/results](#handling-optionsresults)).

```v wip
ch := chan int{}
ch2 := chan f64{}
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
without noticeable CPU load. It consists of a list of possible transfers and associated branches
of statements - similar to the [match](#match) command:

```v
import time

fn main() {
	ch := chan f64{}
	ch2 := chan f64{}
	ch3 := chan f64{}
	mut b := 0.0
	c := 1.0
	// ... setup spawn threads that will send on ch/ch2
	spawn fn (the_channel chan f64) {
		time.sleep(5 * time.millisecond)
		the_channel <- 1.0
	}(ch)
	spawn fn (the_channel chan f64) {
		time.sleep(1 * time.millisecond)
		the_channel <- 1.0
	}(ch2)
	spawn fn (the_channel chan f64) {
		_ := <-the_channel
	}(ch3)

	select {
		a := <-ch {
			// do something with `a`
			eprintln('> a: ${a}')
		}
		b = <-ch2 {
			// do something with predeclared variable `b`
			eprintln('> b: ${b}')
		}
		ch3 <- c {
			// do something if `c` was sent
			time.sleep(5 * time.millisecond)
			eprintln('> c: ${c} was send on channel ch3')
		}
		500 * time.millisecond {
			// do something if no channel has become ready within 0.5s
			eprintln('> more than 0.5s passed without a channel being ready')
		}
	}
	eprintln('> done')
}
```

The timeout branch is optional. If it is absent `select` waits for an unlimited amount of time.
It is also possible to proceed immediately if no channel is ready in the moment `select` is called
by adding an `else { ... }` branch. `else` and `<timeout>` are mutually exclusive.

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

For special purposes there are some builtin fields and methods:

```v
ch := chan int{cap: 2}
println(ch.try_push(42)) // `.success` if pushed, `.not_ready` if full, `.closed` if closed
println(ch.len) // Number of items in the buffer
println(ch.cap) // Buffer capacity
println(ch.closed) // Whether the channel is closed
```

```v
struct Abc {
	x int
}

a := 2.13
ch := chan f64{}
res := ch.try_push(a) // try to perform `ch <- a`
println(res)
l := ch.len // number of elements in queue
c := ch.cap // maximum queue length
is_closed := ch.closed // bool flag - has `ch` been closed
println(l)
println(c)
mut b := Abc{}
ch2 := chan Abc{}
res2 := ch2.try_pop(mut b) // try to perform `b = <-ch2`
```

The `try_push/pop()` methods will return immediately with one of the results
`.success`, `.not_ready` or `.closed` - dependent on whether the object has been transferred or
the reason why not.
Usage of these methods and fields in production is not recommended -
algorithms based on them are often subject to race conditions. Especially `.len` and
`.closed` should not be used to make decisions.
Use `or` branches, error propagation or `select` instead (see [Syntax and Usage](#syntax-and-usage)
and [Channel Select](#channel-select) above).

### Shared Objects

Data can be exchanged between a thread and the calling thread via a shared variable.
Such variables should be created as `shared` and passed to the thread as such, too.
The underlying `struct` contains a hidden *mutex* that allows locking concurrent access
using `rlock` for read-only and `lock` for read/write access.

Note: Shared variables must be structs, arrays or maps.

#### Example of Shared Objects

```v
struct Counter {
mut:
	value int
}

fn (shared counter Counter) increment() {
	lock counter {
		counter.value += 1
		println('Incremented to: ${counter.value}')
	}
}

fn main() {
	shared counter := Counter{}

	spawn counter.increment()
	spawn counter.increment()

	rlock counter {
		println('Final value: ${counter.value}')
	}
}
```

### Difference Between Channels and Shared Objects

**Purpose**:
- Channels: Used for message passing between threads, ensuring safe communication.
- Shared objects: Used for direct data sharing and modification between threads.

**Synchronization**:
- Channels: Implicit (via channel operations)
- Shared objects:  Explicit (via `rlock`/`lock` blocks)

## JSON

Because of the ubiquitous nature of JSON, support for it is built directly into V.

V generates code for JSON encoding and decoding.
No runtime reflection is used. This results in much better performance.

### Decoding JSON

```v
import json

struct Foo {
	x int
}

struct User {
	// Adding a [required] attribute will make decoding fail, if that
	// field is not present in the input.
	// If a field is not [required], but is missing, it will be assumed
	// to have its default value, like 0 for numbers, or '' for strings,
	// and decoding will not fail.
	name string @[required]
	age  int
	// Use the `@[skip]` attribute to skip certain fields.
	// You can also use `@[json: '-']`, and `@[sql: '-']`, which will cause only
	// the `json` module to skip the field, or only the SQL orm to skip it.
	foo Foo @[skip]
	// If the field name is different in JSON, it can be specified
	last_name string @[json: lastName]
}

data := '{ "name": "Frodo", "lastName": "Baggins", "age": 25, "nullable": null }'
user := json.decode(User, data) or {
	eprintln('Failed to decode json, error: ${err}')
	return
}
println(user.name)
println(user.last_name)
println(user.age)
// You can also decode JSON arrays:
sfoos := '[{"x":123},{"x":456}]'
foos := json.decode([]Foo, sfoos)!
println(foos[0].x)
println(foos[1].x)
```

The `json.decode` function takes two arguments:
the first is the type into which the JSON value should be decoded and
the second is a string containing the JSON data.

### Encoding JSON

```v
import json

struct User {
	name  string
	score i64
}

mut data := map[string]int{}
user := &User{
	name:  'Pierre'
	score: 1024
}

data['x'] = 42
data['y'] = 360

println(json.encode(data)) // {"x":42,"y":360}
println(json.encode(user)) // {"name":"Pierre","score":1024}
```

The json module also supports anonymous struct fields, which helps with complex JSON apis with lots
of levels.

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
the program will usually abort. Asserts should only be used to detect programming errors. When an
assert fails it is reported to *stderr*, and the values on each side of a comparison operator
(such as `<`, `==`) will be printed when possible. This is useful to easily find an
unexpected value. Assert statements can be used in any function, not just test ones,
which is handy when developing new functionality, to keep your invariants in check.

> [!NOTE]
> All `assert` statements are *removed*, when you compile your program with the `-prod` flag.

### Asserts with an extra message

This form of the `assert` statement, will print the extra message when it fails. Note that
you can use any string expression there - string literals, functions returning a string,
strings that interpolate variables, etc.

```v
fn test_assertion_with_extra_message_failure() {
	for i in 0 .. 100 {
		assert i * 2 - 45 < 75 + 10, 'assertion failed for i: ${i}'
	}
}
```

### Asserts that do not abort your program

When initially prototyping functionality and tests, it is sometimes desirable to
have asserts that do not stop the program, but just print their failures. That can
be achieved by tagging your assert containing functions with an `[assert_continues]`
tag, for example running this program:

```v
@[assert_continues]
fn abc(ii int) {
	assert ii == 2
}

for i in 0 .. 4 {
	abc(i)
}
```

... will produce this output:

```
assert_continues_example.v:3: FAIL: fn main.abc: assert ii == 2
   left value: ii = 0
   right value: 2
assert_continues_example.v:3: FAIL: fn main.abc: assert ii == 2
   left value: ii = 1
  right value: 2
assert_continues_example.v:3: FAIL: fn main.abc: assert ii == 2
   left value: ii = 3
  right value: 2
```

> [!NOTE]
> V also supports a command line flag `-assert continues`, which will change the
> behaviour of all asserts globally, as if you had tagged every function with `[assert_continues]`.

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
// hello_test.v
module main

fn test_hello() {
	assert hello() == 'Hello world'
}
```

To run the test file above, use `v hello_test.v`. This will check that the function `hello` is
producing the correct output. V executes all test functions in the file.

> [!NOTE]
> All `_test.v` files (both external and internal ones), are compiled as *separate programs*.
> In other words, you may have as many `_test.v` files, and tests in them as you like, they will
> not affect the compilation of your other code in `.v` files normally at all, but only when you
> do explicitly `v file_test.v` or `v test .`.

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

In the example above, `test_hello` is an internal test that can call
the private function `hello()` because `hello_test.v` has `module main`,
just like `hello.v`, i.e. both are part of the same module. Note also that
since `module main` is a regular module like the others, internal tests can
be used to test private functions in your main program .v files too.

You can also define these special test functions in a test file:

* `testsuite_begin` which will be run *before* all other test functions.
* `testsuite_end` which will be run *after* all other test functions.

If a test function has an error return type, any propagated errors will fail the test:

```v
import strconv

fn test_atoi() ! {
	assert strconv.atoi('1')! == 1
	assert strconv.atoi('one')! == 1 // test will fail
}
```

### Running tests

To run test functions in an individual test file, use `v foo_test.v`.

To test an entire module, use `v test mymodule`. You can also use `v test .` to test
everything inside your current folder (and subfolders). You can pass the `-stats`
option to see more details about the individual tests run.

You can put additional test data, including .v source files in a folder, named
`testdata`, right next to your _test.v files. V's test framework will *ignore*
such folders, while scanning for tests to run. This is useful, if you want to
put .v files with invalid V source code, or other tests, including known
failing ones, that should be run in a specific way/options by a parent _test.v
file.

> [!NOTE]
> The path to the V compiler, is available through @VEXE, so a _test.v
> file, can easily run *other* test files like this:

```v oksyntax
import os

fn test_subtest() {
	res := os.execute('${os.quoted_path(@VEXE)} other_test.v')
	assert res.exit_code == 1
	assert res.output.contains('other_test.v does not exist')
}
```

## Memory management

V avoids doing unnecessary allocations in the first place by using value types,
string buffers, promoting a simple abstraction-free code style.

There are 4 ways to manage memory in V.

The default is a minimal and a well performing tracing GC.

The second way is autofree, it can be enabled with `-autofree`. It takes care of most objects
(~90-100%): the compiler inserts necessary free calls automatically during compilation.
Remaining small percentage of objects is freed via GC. The developer doesn't need to change
anything in their code. "It just works", like in Python, Go, or Java, except there's no
heavy GC tracing everything or expensive RC for each object.

For developers willing to have more low level control, memory can be managed manually with
`-gc none`.

Arena allocation is available via v `-prealloc`.

### Control

You can take advantage of V's autofree engine and define a `free()` method on custom
data types:

```v
struct MyType {}

@[unsafe]
fn (data &MyType) free() {
	// ...
}
```

Just as the compiler frees C data types with C's `free()`, it will statically insert
`free()` calls for your data type at the end of each variable's lifetime.

Autofree can be enabled with an `-autofree` flag.

For developers willing to have more low level control, autofree can be disabled with
`-manualfree`, or by adding a `[manualfree]` on each function that wants to manage its
memory manually. (See [attributes](#attributes)).

> [!NOTE]
> Autofree is still WIP. Until it stabilises and becomes the default, please
> avoid using it. Right now allocations are handled by a minimal and well performing GC
> until V's autofree engine is production ready.

**Examples**

```v
import strings

fn draw_text(s string, x int, y int) {
	// ...
}

fn draw_scene() {
	// ...
	name1 := 'abc'
	name2 := 'def ghi'
	draw_text('hello ${name1}', 10, 10)
	draw_text('hello ${name2}', 100, 10)
	draw_text(strings.repeat(`X`, 10000), 10, 50)
	// ...
}
```

The strings don't escape `draw_text`, so they are cleaned up when
the function exits.

In fact, with the `-prealloc` flag, the first two calls won't result in any allocations at all.
These two strings are small, so V will use a preallocated buffer for them.

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

### Stack and Heap

#### Stack and Heap Basics

Like with most other programming languages there are two locations where data can
be stored:

* The *stack* allows fast allocations with almost zero administrative overhead. The
  stack grows and shrinks with the function call depth &ndash; so every called
  function has its stack segment that remains valid until the function returns.
  No freeing is necessary, however, this also means that a reference to a stack
  object becomes invalid on function return. Furthermore stack space is
  limited (typically to a few Megabytes per thread).
* The *heap* is a large memory area (typically some Gigabytes) that is administrated
  by the operating system. Heap objects are allocated and freed by special function
  calls that delegate the administrative tasks to the OS. This means that they can
  remain valid across several function calls, however, the administration is
  expensive.

#### V's default approach

Due to performance considerations V tries to put objects on the stack if possible
but allocates them on the heap when obviously necessary. Example:

```v
struct MyStruct {
	n int
}

struct RefStruct {
	r &MyStruct
}

fn main() {
	q, w := f()
	println('q: ${q.r.n}, w: ${w.n}')
}

fn f() (RefStruct, &MyStruct) {
	a := MyStruct{
		n: 1
	}
	b := MyStruct{
		n: 2
	}
	c := MyStruct{
		n: 3
	}
	e := RefStruct{
		r: &b
	}
	x := a.n + c.n
	println('x: ${x}')
	return e, &c
}
```

Here `a` is stored on the stack since its address never leaves the function `f()`.
However a reference to `b` is part of `e` which is returned. Also a reference to
`c` is returned. For this reason `b` and `c` will be heap allocated.

Things become less obvious when a reference to an object is passed as a function argument:

```v
struct MyStruct {
mut:
	n int
}

fn main() {
	mut q := MyStruct{
		n: 7
	}
	w := MyStruct{
		n: 13
	}
	x := q.f(&w) // references of `q` and `w` are passed
	println('q: ${q}\nx: ${x}')
}

fn (mut a MyStruct) f(b &MyStruct) int {
	a.n += b.n
	x := a.n * b.n
	return x
}
```

Here the call `q.f(&w)` passes references to `q` and `w` because `a` is
`mut` and `b` is of type `&MyStruct` in `f()`'s declaration, so technically
these references are leaving `main()`. However the *lifetime* of these
references lies inside the scope of `main()` so `q` and `w` are allocated
on the stack.

#### Manual Control for Stack and Heap

In the last example the V compiler could put `q` and `w` on the stack
because it assumed that in the call `q.f(&w)` these references were only
used for reading and modifying the referred values &ndash; and not to pass the
references themselves somewhere else. This can be seen in a way that the
references to `q` and `w` are only *borrowed* to `f()`.

Things become different if `f()` is doing something with a reference itself:

```v
struct RefStruct {
mut:
	r &MyStruct
}

// see discussion below
@[heap]
struct MyStruct {
	n int
}

fn main() {
	mut m := MyStruct{}
	mut r := RefStruct{
		r: &m
	}
	r.g()
	println('r: ${r}')
}

fn (mut r RefStruct) g() {
	s := MyStruct{
		n: 7
	}
	r.f(&s) // reference to `s` inside `r` is passed back to `main() `
}

fn (mut r RefStruct) f(s &MyStruct) {
	r.r = s // would trigger error without `[heap]`
}
```

Here `f()` looks quite innocent but is doing nasty things &ndash; it inserts a
reference to `s` into `r`. The problem with this is that `s` lives only as long
as `g()` is running but `r` is used in `main()` after that. For this reason
the compiler would complain about the assignment in `f()` because `s` *"might
refer to an object stored on stack"*. The assumption made in `g()` that the call
`r.f(&s)` would only borrow the reference to `s` is wrong.

A solution to this dilemma is the `[heap]` [attribute](#attributes) at the declaration of
`struct MyStruct`. It instructs the compiler to *always* allocate `MyStruct`-objects
on the heap. This way the reference to `s` remains valid even after `g()` returns.
The compiler takes into consideration that `MyStruct` objects are always heap
allocated when checking `f()` and allows assigning the reference to `s` to the
`r.r` field.

There is a pattern often seen in other programming languages:

```v failcompile
fn (mut a MyStruct) f() &MyStruct {
	// do something with a
	return &a // would return address of borrowed object
}
```

Here `f()` is passed a reference `a` as receiver that is passed back to the caller and returned
as result at the same time. The intention behind such a declaration is method chaining like
`y = x.f().g()`. However, the problem with this approach is that a second reference
to `a` is created &ndash; so it is not only borrowed and `MyStruct` has to be
declared as `[heap]`.

In V the better approach is:

```v
struct MyStruct {
mut:
	n int
}

fn (mut a MyStruct) f() {
	// do something with `a`
}

fn (mut a MyStruct) g() {
	// do something else with `a`
}

fn main() {
	x := MyStruct{} // stack allocated
	mut y := x
	y.f()
	y.g()
	// instead of `mut y := x.f().g()
}
```

This way the `[heap]` attribute can be avoided &ndash; resulting in better performance.

However, stack space is very limited as mentioned above. For this reason the `[heap]`
attribute might be suitable for very large structures even if not required by use cases
like those mentioned above.

There is an alternative way to manually control allocation on a case to case basis. This
approach is not recommended but shown here for the sake of completeness:

```v
struct MyStruct {
	n int
}

struct RefStruct {
mut:
	r &MyStruct
}

// simple function - just to overwrite stack segment previously used by `g()`

fn use_stack() {
	x := 7.5
	y := 3.25
	z := x + y
	println('${x} ${y} ${z}')
}

fn main() {
	mut m := MyStruct{}
	mut r := RefStruct{
		r: &m
	}
	r.g()
	use_stack() // to erase invalid stack contents
	println('r: ${r}')
}

fn (mut r RefStruct) g() {
	s := &MyStruct{ // `s` explicitly refers to a heap object
		n: 7
	}
	// change `&MyStruct` -> `MyStruct` above and `r.f(s)` -> `r.f(&s)` below
	// to see data in stack segment being overwritten
	r.f(s)
}

fn (mut r RefStruct) f(s &MyStruct) {
	r.r = unsafe { s } // override compiler check
}
```

Here the compiler check is suppressed by the `unsafe` block. To make `s` be heap
allocated even without `[heap]` attribute the `struct` literal is prefixed with
an ampersand: `&MyStruct{...}`.

This last step would not be required by the compiler but without it the reference
inside `r` becomes invalid (the memory area pointed to will be overwritten by
`use_stack()`) and the program might crash (or at least produce an unpredictable
final output). That's why this approach is *unsafe* and should be avoided!

## ORM

(This is still in an alpha state)

V has a built-in ORM (object-relational mapping) which supports SQLite, MySQL and Postgres,
but soon it will support MS SQL and Oracle.

V's ORM provides a number of benefits:

- One syntax for all SQL dialects. (Migrating between databases becomes much easier.)
- Queries are constructed using V's syntax. (There's no need to learn another syntax.)
- Safety. (All queries are automatically sanitised to prevent SQL injection.)
- Compile time checks. (This prevents typos which can only be caught during runtime.)
- Readability and simplicity. (You don't need to manually parse the results of a query and
  then manually construct objects from the parsed results.)

```v
import db.sqlite

// sets a custom table name. Default is struct name (case-sensitive)
@[table: 'customers']
struct Customer {
	id        int @[primary; serial] // a field named `id` of integer type must be the first field
	name      string
	nr_orders int
	country   ?string
}

db := sqlite.connect('customers.db')!

// You can create tables from your struct declarations. For example the next query will issue SQL similar to this:
// CREATE TABLE IF NOT EXISTS `Customer` (
//      `id` INTEGER PRIMARY KEY,
//      `name` TEXT NOT NULL,
//      `nr_orders` INTEGER NOT NULL,
//      `country` TEXT
// )
sql db {
	create table Customer
}!

// insert a new customer:
new_customer := Customer{
	name:      'Bob'
	country:   'uk'
	nr_orders: 10
}
sql db {
	insert new_customer into Customer
}!

us_customer := Customer{
	name:      'Martin'
	country:   'us'
	nr_orders: 5
}
sql db {
	insert us_customer into Customer
}!

none_country_customer := Customer{
	name:      'Dennis'
	country:   none
	nr_orders: 2
}
sql db {
	insert none_country_customer into Customer
}!

// update a customer:
sql db {
	update Customer set nr_orders = nr_orders + 1 where name == 'Bob'
}!

// select count(*) from customers
nr_customers := sql db {
	select count from Customer
}!
println('number of all customers: ${nr_customers}')

// V's syntax can be used to build queries:
uk_customers := sql db {
	select from Customer where country == 'uk' && nr_orders > 0
}!
println('We found a total of ${uk_customers.len} customers matching the query.')
for c in uk_customers {
	println('customer: ${c.id}, ${c.name}, ${c.country}, ${c.nr_orders}')
}

none_country_customers := sql db {
	select from Customer where country is none
}!
println('We found a total of ${none_country_customers.len} customers, with no country set.')
for c in none_country_customers {
	println('customer: ${c.id}, ${c.name}, ${c.country}, ${c.nr_orders}')
}

// delete a customer
sql db {
	delete from Customer where name == 'Bob'
}!
```

For more examples and the docs, see [vlib/orm](https://github.com/vlang/v/tree/master/vlib/orm).

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

### Newlines in Documentation Comments

Comments spanning multiple lines are merged together using spaces, unless

- the line is empty
- the line is purely of at least 3 of `-`, `=`, `_`, `*`, `~` (horizontal rule)
- the line starts with at least one `#` followed by a space (header)
- the line starts and ends with a `|` (table)
- the line starts with `- ` (list)

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

#### Disabling the formatting locally

To disable formatting for a block of code, wrap it with `// vfmt off` and
`// vfmt on` comments.

```bash
// Not affected by fmt
// vfmt off

... your code here ...

// vfmt on

// Affected by fmt
... your code here ...
```

### v shader

You can use GPU shaders with V graphical apps. You write your shaders in an
[annotated GLSL dialect](https://github.com/vlang/v/blob/master/examples/sokol/02_cubes_glsl/cube_glsl.glsl)
and use `v shader` to compile them for all supported target platforms.

```shell
v shader /path/to/project/dir/or/file.v
```

Currently you need to
[include a header and declare a glue function](https://github.com/vlang/v/blob/master/examples/sokol/02_cubes_glsl/cube_glsl.v#L25-L28)
before using the shader in your code.

### Profiling

V has good support for profiling your programs: `v -profile profile.txt run file.v`
That will produce a profile.txt file, which you can then analyze.

The generated profile.txt file will have lines with 4 columns:

1. How many times a function was called.
2. How much time in total a function took (in ms).
3. How much time a function took (in ms), on its own, without the calls inside it.
   It is reliable for multithreaded programs, when tcc is not used.
4. How much time on average, a call to a function took (in ns).
5. The name of the v function.

You can sort on column 3 (average time per function) using:
`sort -n -k3 profile.txt|tail`

You can also use stopwatches to measure just portions of your code explicitly:

```v
import time

fn main() {
	sw := time.new_stopwatch()
	println('Hello world')
	println('Greeting the world took: ${sw.elapsed().nanoseconds()}ns')
}
```

## Package management

A V *module* is a single folder with .v files inside. A V *package* can
contain one or more V modules. A V *package* should have a `v.mod` file
at its top folder, describing the contents of the package.

V packages are installed normally in your `~/.vmodules` folder. That
location can be overridden by setting the env variable `VMODULES`.

### Package commands

You can use the V frontend to do package operations, just like you can
use it for compiling code, formatting code, vetting code etc.

```powershell
v [package_command] [param]
```

where a package command can be one of:

```
   install           Install a package from VPM.
   remove            Remove a package that was installed from VPM.
   search            Search for a package from VPM.
   update            Update an installed package from VPM.
   upgrade           Upgrade all the outdated packages.
   list              List all installed packages.
   outdated          Show installed packages that need updates.
```

You can install packages already created by someone else with [VPM](https://vpm.vlang.io/):

```powershell
v install [package]
```

**Example:**

```powershell
v install ui
```

Packages can be installed directly from git or mercurial repositories.

```powershell
v install [--once] [--git|--hg] [url]
```

**Example:**

```powershell
v install --git https://github.com/vlang/markdown
```

Sometimes you may want to install the dependencies **ONLY** if those are not installed:

```
v install --once [package]
```

Removing a package with v:

```powershell
v remove [package]
```

**Example:**

```powershell
v remove ui
```

Updating an installed package from [VPM](https://vpm.vlang.io/):

```powershell
v update [package]
```

**Example:**

```powershell
v update ui
```

Or you can update all your packages:

```powershell
v update
```

To see all the packages you have installed, you can use:

```powershell
v list
```

**Example:**

```powershell
> v list
Installed packages:
  markdown
  ui
```

To see all the packages that need updates:

```powershell
v outdated
```

**Example:**

```powershell
> v outdated
Package are up to date.
```

### Publish package

1. Put a `v.mod` file inside the toplevel folder of your package (if you
   created your package with the command `v new mypackage` or `v init`
   you already have a `v.mod` file).

   ```sh
   v new mypackage
   Input your project description: My nice package.
   Input your project version: (0.0.0) 0.0.1
   Input your project license: (MIT)
   Initialising ...
   Complete!
   ```

   Example `v.mod`:
   ```v ignore
   Module {
       name: 'mypackage'
       description: 'My nice package.'
       version: '0.0.1'
       license: 'MIT'
       dependencies: []
   }
   ```

   Minimal file structure:
   ```
   v.mod
   mypackage.v
   ```

   The name of your package should be used with the `module` directive
   at the top of all files in your package. For `mypackage.v`:
   ```v
   module mypackage

   pub fn hello_world() {
       println('Hello World!')
   }
   ```

2. Create a git repository in the folder with the `v.mod` file
   (this is not required if you used `v new` or `v init`):
   ```sh
   git init
   git add .
   git commit -m "INIT"
   ````

3. Create a public repository on github.com.
4. Connect your local repository to the remote repository and push the changes.
5. Add your package to the public V package registry VPM:
   https://vpm.vlang.io/new

   You will have to login with your Github account to register the package.
   **Warning:** _Currently it is not possible to edit your entry after submitting.
   Check your package name and github url twice as this cannot be changed by you later._
6. The final package name is a combination of your github account and
   the package name you provided e.g. `mygithubname.mypackage`.

**Optional:** tag your V package with `vlang` and `vlang-package` on github.com
to allow for a better search experience.

# Advanced Topics

## Attributes

V has several attributes that modify the behavior of functions and structs.

An attribute is a compiler instruction specified inside `[]` right before a
function/struct/enum declaration and applies only to the following declaration.

```v
// @[flag] enables Enum types to be used as bitfields

@[flag]
enum BitField {
	read
	write
	other
}

fn main() {
	assert 1 == int(BitField.read)
	assert 2 == int(BitField.write)
	mut bf := BitField.read
	assert bf.has(.read | .other) // test if *at least one* of the flags is set
	assert !bf.all(.read | .other) // test if *all* of the flags are set
	bf.set(.write | .other)
	assert bf.has(.read | .write | .other)
	assert bf.all(.read | .write | .other)
	bf.toggle(.other)
	assert bf == BitField.read | .write
	assert bf.all(.read | .write)
	assert !bf.has(.other)
	empty := BitField.zero()
	assert empty.is_empty()
	assert !empty.has(.read)
	assert !empty.has(.write)
	assert !empty.has(.other)
	mut full := empty
	full.set_all()
	assert int(full) == 7 // 0x01 + 0x02 + 0x04
	assert full == .read | .write | .other
	mut v := full
	v.clear(.read | .other)
	assert v == .write
	v.clear_all()
	assert v == empty
	assert BitField.read == BitField.from('read')!
	assert BitField.other == BitField.from('other')!
	assert BitField.write == BitField.from(2)!
	assert BitField.zero() == BitField.from('')!
}
```

```v
// @[_allow_multiple_values] allows an enum to have multiple duplicate values.
// Use it carefully, only when you really need it.

@[_allow_multiple_values]
enum ButtonStyle {
	primary   = 1
	secondary = 2
	success   = 3

	blurple = 1
	grey    = 2
	gray    = 2
	green   = 3
}

fn main() {
	assert int(ButtonStyle.primary) == 1
	assert int(ButtonStyle.blurple) == 1

	assert int(ButtonStyle.secondary) == 2
	assert int(ButtonStyle.gray) == 2
	assert int(ButtonStyle.grey) == 2

	assert int(ButtonStyle.success) == 3
	assert int(ButtonStyle.green) == 3

	assert ButtonStyle.primary == ButtonStyle.blurple
	assert ButtonStyle.secondary == ButtonStyle.grey
	assert ButtonStyle.secondary == ButtonStyle.gray
	assert ButtonStyle.success == ButtonStyle.green
}
```

Struct field deprecations:

```v oksyntax
module abc

// Note that only *direct* accesses to Xyz.d in *other modules*, will produce deprecation notices/warnings:
pub struct Xyz {
pub mut:
	a int
	d int @[deprecated: 'use Xyz.a instead'; deprecated_after: '2999-03-01']
	// the tags above, will produce a notice, since the deprecation date is in the far future
}
```

Function/method deprecations:

Functions are deprecated before they are finally removed to give users time to migrate their code.
Adding a date is preferable in most cases. An immediate change, without a deprecation date, may be
used for functions that are found to be conceptually broken and obsoleted by much better
functionality. Other than that setting a date is advisable to grant users a grace period.

Deprecated functions cause warnings, which cause errors if built with `-prod`. To avoid immediate
CI breakage, it is advisable to set a future date, ahead of the date when the code is merged. This
gives people who actively developed V projects, the chance to see the deprecation notice at least
once and fix the uses. Setting a date in the next 30 days, assumes they would have compiled their
projects manually at least once, within that time. For small changes, this should be plenty
of time. For complex changes, this time may need to be longer.

Different V projects and maintainers may reasonably choose different deprecation policies.
Depending on the type and impact of the change, you may want to consult with them first, before
deprecating a function.


```v
// Calling this function will result in a deprecation warning

@[deprecated]
fn old_function() {
}

// It can also display a custom deprecation message

@[deprecated: 'use new_function() instead']
fn legacy_function() {}

// You can also specify a date, after which the function will be
// considered deprecated. Before that date, calls to the function
// will be compiler notices - you will see them, but the compilation
// is not affected. After that date, calls will become warnings,
// so ordinary compiling will still work, but compiling with -prod
// will not (all warnings are treated like errors with -prod).
// 6 months after the deprecation date, calls will be hard
// compiler errors.

@[deprecated: 'use new_function2() instead']
@[deprecated_after: '2021-05-27']
fn legacy_function2() {}
```

```v globals
// This function's calls will be inlined.
@[inline]
fn inlined_function() {
}

// This function's calls will NOT be inlined.
@[noinline]
fn function() {
}

// This function will NOT return to its callers.
// Such functions can be used at the end of or blocks,
// just like exit/1 or panic/1. Such functions can not
// have return types, and should end either in for{}, or
// by calling other `[noreturn]` functions.
@[noreturn]
fn forever() {
	for {}
}

// The following struct must be allocated on the heap. Therefore, it can only be used as a
// reference (`&Window`) or inside another reference (`&OuterStruct{ Window{...} }`).
// See section "Stack and Heap"
@[heap]
struct Window {
}

// Calls to following function must be in unsafe{} blocks.
// Note that the code in the body of `risky_business()` will still be
// checked, unless you also wrap it in `unsafe {}` blocks.
// This is useful, when you want to have an `[unsafe]` function that
// has checks before/after a certain unsafe operation, that will still
// benefit from V's safety features.
@[unsafe]
fn risky_business() {
	// code that will be checked, perhaps checking pre conditions
	unsafe {
		// code that *will not be* checked, like pointer arithmetic,
		// accessing union fields, calling other `[unsafe]` fns, etc...
		// Usually, it is a good idea to try minimizing code wrapped
		// in unsafe{} as much as possible.
		// See also [Memory-unsafe code](#memory-unsafe-code)
	}
	// code that will be checked, perhaps checking post conditions and/or
	// keeping invariants
}

// V's autofree engine will not take care of memory management in this function.
// You will have the responsibility to free memory manually yourself in it.
// Note: it is NOT related to the garbage collector. It will only make the
// -autofree mechanism, ignore the body of that function.
@[manualfree]
fn custom_allocations() {
}

// The memory pointed to by the pointer arguments of this function will not be
// freed by the garbage collector (if in use) before the function returns
// For C interop only.
@[keep_args_alive]
fn C.my_external_function(voidptr, int, voidptr) int

// A @[weak] tag tells the C compiler, that the next declaration will be weak, i.e. when linking,
// if there is another declaration of a symbol with the same name (a 'strong' one), it should be
// used instead, *without linker errors about duplicate symbols*.
// For C interop only.

@[weak]
__global abc = u64(1)

// Tell V, that the following global was defined on the C side,
// thus V will not initialise it, but will just give you access to it.
// For C interop only.

@[c_extern]
__global my_instance C.my_struct
struct C.my_struct {
	a int
	b f64
}

// Tell V that the following struct is defined with `typedef struct` in C.
// For C interop only.
@[typedef]
pub struct C.Foo {}

// Used to add a custom calling convention to a function, available calling convention: stdcall, fastcall and cdecl.
// This list also applies for type aliases (see below).
// For C interop only.
@[callconv: 'stdcall']
fn C.DefWindowProc(hwnd int, msg int, lparam int, wparam int)

// Used to add a custom calling convention to a function type aliases.
// For C interop only.

@[callconv: 'fastcall']
type FastFn = fn (int) bool

// Calls to the following function, will have to use its return value somehow.
// Ignoring it, will emit warnings.
@[must_use]
fn f() int {
	return 42
}

fn g() {
	// just calling `f()` here, will produce a warning
	println(f()) // this is fine, because the return value was used as an argument
}

// Windows only (and obsolete; instead of it, use `-subsystem windows` when compiling)
// Without this attribute all graphical apps will have the following behavior on Windows:
// If run from a console or terminal; keep the terminal open so all (e)println statements can be viewed.
// If run from e.g. Explorer, by double-click; app is opened, but no terminal is opened, and no
// (e)println output can be seen.
// Use it to force-open a terminal to view output in, even if the app is started from Explorer.
// Valid before main() only.
@[console]
fn main() {
}
```

## Conditional compilation

The goal of this feature, is to tell V to *not compile* a function, and all its calls, in the final
executable, if a provided custom flag is not passed.

V will still type check the function and all its calls, *even* if they will not be present in the
final executable, due to the passed -d flags.

In order to see it in action, run the following example with `v run example.v` once,
and then a second time with `v -d trace_logs example.v`:
```v
@[if trace_logs ?]
fn elog(s string) {
	eprintln(s)
}

fn main() {
	elog('some expression: ${2 + 2}') // such calls will not be done *at all*, if `-d trace_logs` is not passed
	println('hi')
	elog('finish')
}
```

Conditional compilation, based on custom flags, can also be used to produce slightly different
executables, which share the majority of the same code, but where some of the logic, is needed
only some of the time, for example a network server/client program can be written like so:
```v ignore
fn act_as_client() { ... }
fn act_as_server() { ... }
fn main() {
	$if as_client ? {
		act_as_client()
	}
	$if as_server ? {
		act_as_server()
	}
}
```
To generate a `client.exe` executable do: `v -d as_client -o client.exe .`
To generate a `server.exe` executable do: `v -d as_server -o server.exe .`

### Compile time pseudo variables

V also gives your code access to a set of pseudo string variables,
that are substituted at compile time:

- `@FN` => replaced with the name of the current V function.
- `@METHOD` => replaced with ReceiverType.MethodName.
- `@MOD` => replaced with the name of the current V module.
- `@STRUCT` => replaced with the name of the current V struct.
- `@FILE` => replaced with the absolute path of the V source file.
- `@DIR` => replaced with the absolute path of the *folder*, where the V source file is.
- `@LINE` => replaced with the V line number where it appears (as a string).
- `@FILE_LINE` => like `@FILE:@LINE`, but the file part is a relative path.
- `@LOCATION` => file, line and name of the current type + method; suitable for logging.
- `@COLUMN` => replaced with the column where it appears (as a string).
- `@VEXE` => replaced with the path to the V compiler.
- `@VEXEROOT`  => will be substituted with the *folder*,
  where the V executable is (as a string).
- `@VHASH`  => replaced with the shortened commit hash of the V compiler (as a string).
- `@VCURRENTHASH` => Similar to `@VHASH`, but changes when the compiler is
  recompiled on a different commit (after local modifications, or after
  using git bisect etc).
- `@VMOD_FILE` => replaced with the contents of the nearest v.mod file (as a string).
- `@VMODHASH` => is replaced by the shortened commit hash, derived from the .git directory
  next to the nearest v.mod file (as a string).
- `@VMODROOT` => will be substituted with the *folder*,
  where the nearest v.mod file is (as a string).
- `@BUILD_DATE` => replaced with the build date, for example '2024-09-13' .
- `@BUILD_TIME` => replaced with the build time, for example '12:32:07' .
- `@BUILD_TIMESTAMP` => replaced with the build timestamp, for example '1726219885' .
Note: `@BUILD_DATE`, `@BUILD_TIME`, `@BUILD_TIMESTAMP` represent times in the UTC timezone.
By default, they are based on the current time of the compilation/build. They can be overridden
by setting the environment variable `SOURCE_DATE_EPOCH`. That is also useful while making
releases, since you can use the equivalent of this in your build system/script:
`export SOURCE_DATE_EPOCH=$(git log -1 --pretty=%ct) ;` , and then use `@BUILD_DATE` etc.,
inside your program, when you for example print your version information to users.
See also https://reproducible-builds.org/docs/source-date-epoch/ .

The compile time pseudo variables allow you to do the following
example, which is useful while debugging/logging/tracing your code:

```v
eprintln(@LOCATION)
```

Another example, is if you want to embed the version/name from v.mod *inside* your executable:

```v ignore
import v.vmod
vm := vmod.decode( @VMOD_FILE ) or { panic(err) }
eprintln('${vm.name} ${vm.version}\n ${vm.description}')
```

A program that prints its own source code (a quine):
```v
print($embed_file(@FILE).to_string())
```

A program that prints the time when it was built:
```v
import time

println('This program, was compiled at ${time.unix(@BUILD_TIMESTAMP.i64()).format_ss_milli()} .')
```

> [!NOTE]
> you can have arbitrary source code in the file, without problems, since the full file
> will be embedded into the executable, produced by compiling it. Also note that printing
> is done with `print` and not `println`, to not add another new line, missing in the
> source code.


### Compile time reflection

`$` is used as a prefix for compile time (also referred to as 'comptime') operations.

Having built-in JSON support is nice, but V also allows you to create efficient
serializers for any data format. V has compile time `if` and `for` constructs:

#### <h4 id="comptime-fields">.fields</h4>

You can iterate over struct fields using `.fields`, it also works with generic types
(e.g. `T.fields`) and generic arguments (e.g. `param.fields` where `fn gen[T](param T) {`).

```v
struct User {
	name string
	age  int
}

fn main() {
	$for field in User.fields {
		$if field.typ is string {
			println('${field.name} is of type string')
		}
	}
}

// Output:
// name is of type string
```

#### <h4 id="comptime-values">.values</h4>

You can read [Enum](#enums) values and their attributes.

```v
enum Color {
	red   @[RED]  // first attribute
	blue  @[BLUE] // second attribute
}

fn main() {
	$for e in Color.values {
		println(e.name)
		println(e.attrs)
	}
}

// Output:
// red
// ['RED']
// blue
// ['BLUE']
```

#### <h4 id="comptime-attrs">.attributes</h4>

You can read [Struct](#structs) attributes.

```v
@[COLOR]
struct Foo {
	a int
}

fn main() {
	$for e in Foo.attributes {
		println(e)
	}
}

// Output:
// StructAttribute{
//    name: 'COLOR'
//    has_arg: false
//    arg: ''
//    kind: plain
// }
```

#### <h4 id="comptime-variants">.variants</h4>

You can read variant types from [Sum type](#sum-types).

```v
type MySum = int | string

fn main() {
	$for v in MySum.variants {
		$if v.typ is int {
			println('has int type')
		} $else $if v.typ is string {
			println('has string type')
		}
	}
}

// Output:
// has int type
// has string type
```

#### <h4 id="comptime-methods">.methods</h4>

You can retrieve information about struct methods.

```v
struct Foo {
}

fn (f Foo) test() int {
	return 123
}

fn (f Foo) test2() string {
	return 'foo'
}

fn main() {
	foo := Foo{}
	$for m in Foo.methods {
		$if m.return_type is int {
			print('${m.name} returns int: ')
			println(foo.$method())
		} $else $if m.return_type is string {
			print('${m.name} returns string: ')
			println(foo.$method())
		}
	}
}

// Output:
// test returns int: 123
// test2 returns string: foo
```

#### <h4 id="comptime-method-params">.params</h4>

You can retrieve information about struct method params.

```v
struct Test {
}

fn (t Test) foo(arg1 int, arg2 string) {
}

fn main() {
	$for m in Test.methods {
		$for param in m.params {
			println('${typeof(param.typ).name}: ${param.name}')
		}
	}
}

// Output:
// int: arg1
// string: arg2
```

See [`examples/compiletime/reflection.v`](/examples/compiletime/reflection.v)
for a more complete example.

### Compile time code

#### `$if` condition

```v
fn main() {
	// Support for multiple conditions in one branch
	$if ios || android {
		println('Running on a mobile device!')
	}
	$if linux && x64 {
		println('64-bit Linux.')
	}
	// Usage as expression
	os := $if windows { 'Windows' } $else { 'UNIX' }
	println('Using ${os}')
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
	// v -prod ...
	$if prod {
		println('production build')
	}
	// v -d option ...
	$if option ? {
		println('custom option')
	}
}
```

If you want an `if` to be evaluated at compile time it must be prefixed with a `$` sign.
Right now it can be used to detect an OS, compiler, platform or compilation options.
`$if debug` is a special option like `$if windows` or `$if x32`, it's enabled if the program
is compiled with `v -g` or `v -cg`.
If you're using a custom ifdef, then you do need `$if option ? {}` and compile with`v -d option`.
Full list of builtin options:

| OS                             | Compilers        | Platforms                     | Other                                         |
|--------------------------------|------------------|-------------------------------|-----------------------------------------------|
| `windows`, `linux`, `macos`    | `gcc`, `tinyc`   | `amd64`, `arm64`, `aarch64`   | `debug`, `prod`, `test`                       |
| `darwin`, `ios`, `bsd`         | `clang`, `mingw` | `i386`, `arm32`               | `js`, `glibc`, `prealloc`                     |
| `freebsd`, `openbsd`, `netbsd` | `msvc`           | `rv64`, `rv32`, `s390x`       | `no_bounds_checking`, `freestanding`          |
| `android`, `mach`, `dragonfly` | `cplusplus`      | `ppc64le`                     | `no_segfault_handler`, `no_backtrace`         |
| `gnu`, `hpux`, `haiku`, `qnx`  |                  | `x64`, `x32`                  | `no_main`, `fast_math`, `apk`, `threads`      |
| `solaris`, `termux`            |                  | `little_endian`, `big_endian` | `js_node`, `js_browser`, `js_freestanding`    |
| `serenity`, `vinix`, `plan9`   |                  |                               | `interpreter`, `es5`, `profile`, `wasm32`     |
|                                |                  |                               | `wasm32_emscripten`, `wasm32_wasi`            |
|                                |                  |                               | `native`, `autofree`                          |

#### `$embed_file`

```v ignore
import os
fn main() {
	embedded_file := $embed_file('v.png')
	os.write_file('exported.png', embedded_file.to_string())!
}
```

V can embed arbitrary files into the executable with the `$embed_file(<path>)`
compile time call. Paths can be absolute or relative to the source file.

Note that by default, using `$embed_file(file)`, will always embed the whole content
of the file, but you can modify that behaviour by passing: `-d embed_only_metadata`
when compiling your program. In that case, the file will not be embedded. Instead,
it will be loaded *the first time* your program calls `embedded_file.data()` at runtime,
making it easier to change in external editor programs, without needing to recompile
your program.

Embedding a file inside your executable, will increase its size, but
it will make it more self contained and thus easier to distribute.
When that happens (the default), `embedded_file.data()` will cause *no IO*,
and it will always return the same data.

`$embed_file` supports compression of the embedded file when compiling with `-prod`.
Currently only one compression type is supported: `zlib`.

```v ignore
import os
fn main() {
	embedded_file := $embed_file('x.css', .zlib) // compressed using zlib
	os.write_file('exported.css', embedded_file.to_string())!
}
```

Note: compressing binary assets like png or zip files, usually will not gain you much,
and in some cases may even take more space in the final executable, since they are
already compressed.

`$embed_file` returns
[EmbedFileData](https://modules.vlang.io/v.embed_file.html#EmbedFileData)
which could be used to obtain the file contents as `string` or `[]u8`.

#### `$tmpl` for embedding and parsing V template files

V has a simple template language for text and html templates, and they can easily
be embedded via `$tmpl('path/to/template.txt')`:

```v ignore
fn build() string {
	name := 'Peter'
	age := 25
	numbers := [1, 2, 3]
	return $tmpl('1.txt')
}

fn main() {
	println(build())
}
```

1.txt:

```
name: @name

age: @age

numbers: @numbers

@for number in numbers
  @number
@end
```

output:

```
name: Peter

age: 25

numbers: [1, 2, 3]

1
2
3
```

See more [details](https://github.com/vlang/v/blob/master/vlib/v/TEMPLATES.md)

#### `$env`

```v
module main

fn main() {
	compile_time_env := $env('ENV_VAR')
	println(compile_time_env)
}
```

V can bring in values at compile time from environment variables.
`$env('ENV_VAR')` can also be used in top-level `#flag` and `#include` statements:
`#flag linux -I $env('JAVA_HOME')/include`.

#### `$d`

V can bring in values at compile time from `-d ident=value` flag defines, passed on
the command line to the compiler. You can also pass `-d ident`, which will have the
same meaning as passing `-d ident=true`.

To get the value in your code, use: `$d('ident', default)`, where `default`
can be `false` for booleans, `0` or `123` for i64 numbers, `0.0` or `113.0`
for f64 numbers, `'a string'` for strings.

When a flag is not provided via the command line, `$d()` will return the `default`
value provided as the *second* argument.

```v
module main

const my_i64 = $d('my_i64', 1024)

fn main() {
	compile_time_value := $d('my_string', 'V')
	println(compile_time_value)
	println(my_i64)
}
```

Running the above with `v run .` will output:
```
V
1024
```

Running the above with `v -d my_i64=4096 -d my_string="V rocks" run .` will output:
```
V rocks
4096
```

Here is an example of how to use the default values, which have to be *pure* literals:
```v
fn main() {
	val_str := $d('id_str', 'value') // can be changed by providing `-d id_str="my id"`
	val_f64 := $d('id_f64', 42.0) // can be changed by providing `-d id_f64=84.0`
	val_i64 := $d('id_i64', 56) // can be changed by providing `-d id_i64=123`
	val_bool := $d('id_bool', false) // can be changed by providing `-d id_bool=true`
	val_char := $d('id_char', `f`) // can be changed by providing `-d id_char=v`
	println(val_str)
	println(val_f64)
	println(val_i64)
	println(val_bool)
	println(rune(val_char))
}
```

`$d('ident','value')` can also be used in top-level statements like `#flag` and `#include`:
`#flag linux -I $d('my_include','/usr')/include`. The default value for `$d` when used in these
statements should be literal `string`s.

`$d('ident', false)` can also be used inside `$if $d('ident', false) {` statements,
granting you the ability to selectively turn on/off certain sections of code, at compile
time, without modifying your source code, or keeping different versions of it.

#### `$compile_error` and `$compile_warn`

These two comptime functions are very useful for displaying custom errors/warnings during
compile time.

Both receive as their only argument a string literal that contains the message to display:

```v failcompile nofmt
// x.v
module main

$if linux {
    $compile_error('Linux is not supported')
}

fn main() {
}

$ v run x.v
x.v:4:5: error: Linux is not supported
    2 |
    3 | $if linux {
    4 |     $compile_error('Linux is not supported')
      |     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    5 | }
    6 |
```

### Compile time types

Compile time types group multiple types into a general higher-level type. This is useful in
functions with generic parameters, where the input type must have a specific property, for example
the `.len` attribute in arrays.

V supports the following compile time types:

- `$alias` => matches [Type aliases](#type-aliases).
- `$array` => matches [Arrays](#arrays) and [Fixed Size Arrays](#fixed-size-arrays).
- `$array_dynamic` => matches [Arrays](#arrays), but not [Fixed Size Arrays](#fixed-size-arrays).
- `$array_fixed` => matches [Fixed Size Arrays](#fixed-size-arrays), but not [Arrays](#arrays)
- `$enum` => matches [Enums](#enums).
- `$float` => matches `f32`, `f64` and float literals.
- `$function` => matches [Function Types](#function-types).
- `$int` => matches `int`, `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `isize`, `usize`
  and integer literals.
- `$interface` => matches [Interfaces](#interfaces).
- `$map` => matches [Maps](#maps).
- `$option` => matches [Option Types](#optionresult-types-and-error-handling).
- `$struct` => matches [Structs](#structs).
- `$sumtype` => matches [Sum Types](#sum-types).
- `$string` => matches [Strings](#strings).

### Environment specific files

If a file has an environment-specific suffix, it will only be compiled for that environment.

- `.js.v` => will be used only by the JS backend. These files can contain JS. code.
- `.c.v` => will be used only by the C backend. These files can contain C. code.
- `.native.v` => will be used only by V's native backend.
- `_nix.c.v` => will be used only on Unix systems (non Windows).
- `_${os}.c.v` => will be used only on the specific `os` system.
  For example, `_windows.c.v` will be used only when compiling on Windows, or with `-os windows`.
- `_default.c.v` => will be used only if there is NOT a more specific platform file.
  For example, if you have both `file_linux.c.v` and `file_default.c.v`,
  and you are compiling for linux, then only `file_linux.c.v` will be used,
  and `file_default.c.v` will be ignored.

Here is a more complete example:

`main.v`:

```v ignore
module main
fn main() { println(message) }
```

`main_default.c.v`:

```v ignore
module main
const message = 'Hello world'
```

`main_linux.c.v`:

```v ignore
module main
const message = 'Hello linux'
```

`main_windows.c.v`:

```v ignore
module main
const message = 'Hello windows'
```

With the example above:

- when you compile for Windows, you will get `Hello windows`
- when you compile for Linux, you will get `Hello linux`
- when you compile for any other platform, you will get the
  non specific `Hello world` message.

- `_d_customflag.v` => will be used *only* if you pass `-d customflag` to V.
  That corresponds to `$if customflag ? {}`, but for a whole file, not just a
  single block. `customflag` should be a snake_case identifier, it can not
  contain arbitrary characters (only lower case latin letters + numbers + `_`).
  > **Note**
  >
  > A combinatorial `_d_customflag_linux.c.v` postfix will not work.
  > If you do need a custom flag file, that has platform dependent code, use the
  > postfix `_d_customflag.v`, and then use platform dependent compile time
  > conditional blocks inside it, i.e. `$if linux {}` etc.

- `_notd_customflag.v` => similar to _d_customflag.v, but will be used
  *only* if you do NOT pass `-d customflag` to V.

See also [Cross Compilation](#cross-compilation).

## Debugger

To use the native *V debugger*, add the `$dbg` statement to your source, where you
want the debugger to be invoked.

```v
fn main() {
	a := 1
	$dbg;
}
```

Running this V code, you will get the debugger REPL break when the execution
reaches the `$dbg` statement.

```
$ v run example.v
Break on [main] main in example.v:3
example.v:3 vdbg>
```

At this point, execution is halted, and the debugger is now available.

To see the available commands, type
?, h or help. (Completion for commands works - Non-Windows only)

```
example.v:3 vdbg> ?
vdbg commands:
  anon?                 check if the current context is anon
  bt                    prints a backtrace
  c, continue           continue debugging
  generic?              check if the current context is generic
  heap                  show heap memory usage
  h, help, ?            show this help
  l, list [lines]       show some lines from current break (default: 3)
  mem, memory           show memory usage
  method?               check if the current context is a method
  m, mod                show current module name
  p, print <var>        prints an variable
  q, quit               exits debugging session in the code
  scope                 show the vars in the current scope
  u, unwatch <var>      unwatches a variable
  w, watch <var>        watches a variable
```

Lets try the `scope` command, to inspect the current scope context.

```
example.v:3 vdbg> scope
a = 1 (int)
```

Cool! We have the variable name, its value and its type name.

What about printing only a variable, not the whole scope?

Just type `p a`.

To watch a variable by its name, use:

`w a` (where `a` is the variable name)

To stop watching the variable (`unwatch` it), use `u a`.

Lets see more one example:

```
fn main() {
	for i := 0; i < 4; i++ {
		$dbg
	}
}
```

Running again, we'll get:
`Break on [main] main in example.v:3`

If we want to read the source code context, we can use the `l` or `list` command.

```
example.v:3 vdbg> l
0001  fn main() {
0002    for i := 0; i < 4; i++ {
0003>           $dbg
0004    }
0005  }
```

The default is read 3 lines before and 3 lines after, but you can
pass a parameter to the command to read more lines, like `l 5`.

Now, lets watch the variable changing on this loop.

```
example.v:3 vdbg> w i
i = 0 (int)
```

To continue to the next breakpoint, type `c` or `continue` command.

```
example.v:3 vdbg> c
Break on [main] main in example.v:3
i = 1 (int)
```

`i` and it's value is automatically printed, because it is in the watch list.

To repeat the last command issued, in this case the `c` command,
just hit the *enter* key.

```
example.v:3 vdbg>
Break on [main] main in example.v:3
i = 2 (int)
example.v:3 vdbg>
Break on [main] main in example.v:3
i = 3 (int)
example.v:3 vdbg>
```

You can also see memory usage with `mem` or `memory` command, and
check if the current context is an anon function (`anon?`), a method (`method?`)
or a generic method (`generic?`) and clear the terminal window (`clear`).

## Call stack

You can also show the current call stack with `v.debug`.

To enable this feature, add the `-d callstack` switch when building or running
your code:

```v
import v.debug

fn test(i int) {
	if i > 9 {
		debug.dump_callstack()
	}
}

fn do_something() {
	for i := 0; i <= 10; i++ {
		test(i)
	}
}

fn main() {
	do_something()
}
```

```
$ v -d callstack run example.v
Backtrace:
--------------------------------------------------
example.v:16   | > main.main
example.v:11   |  > main.do_something
example.v:5    |   > main.test
--------------------------------------------------
```

## Trace

Another feature of `v.debug` is the possibility to add hook functions
before and after each function call.

To enable this feature, add the `-d trace` switch when building or running
your code:

```v
import v.debug

fn main() {
	hook1 := debug.add_before_call(fn (fn_name string) {
		println('> before ${fn_name}')
	})
	hook2 := debug.add_after_call(fn (fn_name string) {
		println('> after ${fn_name}')
	})
	anon := fn () {
		println('call')
	}
	anon()

	// optionally you can remove the hooks:
	debug.remove_before_call(hook1)
	debug.remove_after_call(hook2)
	anon()
}
```

```
$ v -d trace run example.v
> before anon
call
> after anon
call
```

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

> [!NOTE]
> This is work in progress.

## Structs with reference fields

Structs with references require explicitly setting the initial value to a
reference value unless the struct already defines its own initial value.

Zero-value references, or nil pointers, will **NOT** be supported in the future,
for now data structures such as Linked Lists or Binary Trees that rely on reference
fields that can use the value `0`, understanding that it is unsafe, and that it can
cause a panic.

```v
struct Node {
	a &Node
	b &Node = unsafe { nil } // Auto-initialized to nil, use with caution!
}

// Reference fields must be initialized unless an initial value is declared.
// Nil is OK but use with caution, it's a nil pointer.
foo := Node{
	a: unsafe { nil }
}
bar := Node{
	a: &foo
}
baz := Node{
	a: unsafe { nil }
	b: unsafe { nil }
}
qux := Node{
	a: &foo
	b: &bar
}
println(baz)
println(qux)
```

## sizeof and __offsetof

* `sizeof(Type)` gives the size of a type in bytes.
* `__offsetof(Struct, field_name)` gives the offset in bytes of a struct field.

```v
struct Foo {
	a int
	b int
}

assert sizeof(Foo) == 8
assert __offsetof(Foo, a) == 0
assert __offsetof(Foo, b) == 4
```

## Limited operator overloading

Operator overloading defines the behavior of certain binary operators for certain types.

```v
struct Vec {
	x int
	y int
}

fn (a Vec) str() string {
	return '{${a.x}, ${a.y}}'
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
	mut c := Vec{1, 2}

	println(a + b) // "{6, 8}"
	println(a - b) // "{-2, -2}"
	c += a
	//^^ autogenerated from + overload
	println(c) // "{3, 5}"
}
```

> Operator overloading goes against V's philosophy of simplicity and predictability.
> But since scientific and graphical applications are among V's domains,
> operator overloading is an important feature to have in order to improve readability:
>
> `a.add(b).add(c.mul(d))` is a lot less readable than `a + b + c * d`.

Operator overloading is possible for the following binary operators: `+, -, *, /, %, <, ==`.

### Implicitly generated overloads

- `==` is automatically generated by the compiler, but can be overridden.

- `!=`, `>`, `<=` and `>=` are automatically generated when `==` and `<` are defined.
  They cannot be explicitly overridden.
- Assignment operators (`*=`, `+=`, `/=`, etc) are automatically generated when the corresponding
  operators are defined and the operands are of the same type.
  They cannot be explicitly overridden.

### Restriction

To improve safety and maintainability, operator overloading is limited.

#### Type restrictions

- When overriding `<` and `==`, the return type must be strictly `bool`.
- Both arguments must have the same type (just like with all operators in V).
- Overloaded operators have to return the same type as the argument
  (the exceptions are `<` and `==`).

#### Other restrictions

- Arguments cannot be changed inside overloads.
- Calling other functions inside operator functions is not allowed (**planned**).

## Performance tuning

When compiled with `-prod`, V's generated C code usually performs well. However, in specialized
scenarios, additional compiler flags and attributes can further optimize the executable for
performance, memory usage, or size.

> [!NOTE]
> These are *rarely* needed, and should not be used unless you
> *profile your code*, and then see that there are significant benefits for them.
> To cite GCC's documentation: "Programmers are notoriously bad at predicting
> how their programs actually perform".

| Tuning Operation         | Benefits                        | Drawbacks                                         |
|--------------------------|---------------------------------|---------------------------------------------------|
| `@[inline]`              | Performance                     | Increased executable size                         |
| `@[direct_array_access]` | Performance                     | Safety risks                                      |
| `@[packed]`              | Memory usage                    | Potential performance loss                        |
| `@[minify]`              | Performance, Memory usage       | May break binary serialization/reflection         |
| `_likely_/_unlikely_`    | Performance                     | Risk of negative performance impact               |
| `-fast-math`             | Performance                     | Risk of incorrect mathematical operations results |
| `-d no_segfault_handler` | Compile time, Size              | Loss of segfault trace                            |
| `-cflags -march=native`  | Performance                     | Risk of reduced CPU compatibility                 |
| `-compress`              | Size                            | Harder to debug, extra dependency `upx`           |
| `PGO`                    | Performance, Size               | Usage complexity                                  |

### Tuning operations details

#### `@[inline]`

You can tag functions with `@[inline]`, so the C compiler will try to inline them, which in some
cases, may be beneficial for performance, but may impact the size of your executable.

**When to Use**

- Functions that are called frequently in performance-critical loops.

**When to Avoid**

- Large functions, as it might cause code bloat and actually decrease performance.
- Large functions in `if` expressions - may have negative impact on instructions cache.

#### `@[direct_array_access]`

In functions tagged with `@[direct_array_access]` the compiler will translate array operations
directly into C array operations - omitting bounds checking. This may save a lot of time in a
function that iterates over an array but at the cost of making the function unsafe - unless the
boundaries will be checked by the user.

**When to Use**

- In tight loops that access array elements, where bounds have been manually verified or you are
sure that the access index will be valid.

**When to Avoid**

- Everywhere else.

#### `@[packed]`

The `@[packed]` attribute can be applied to a structure to create an unaligned memory layout,
which decreases the overall memory footprint of the structure. Using the `@[packed]` attribute
may negatively impact performance or even be prohibited on certain CPU architectures.

**When to Use**

- When memory usage is more critical than performance, e.g., in embedded systems.

**When to Avoid**

- On CPU architectures that do not support unaligned memory access or when high-speed memory access
is needed.

#### `@[aligned]`

The `@[aligned]` attribute can be applied to a structure or union to specify a minimum alignment
(in bytes) for variables of that type. Using the `@[aligned]` attribute you can only *increase*
the default alignment. Use `@[packed]` if you want to *decrease* it. The alignment of any struct
or union, should be at least a perfect multiple of the lowest common multiple of the alignments of
all of the members of the struct or union.

Example:
```v
// Each u16 in the `data` field below, takes 2 bytes, and we have 3 of them = 6 bytes.
// The smallest power of 2, bigger than 6 is 8, i.e. with `@[aligned]`, the alignment
// for the entire struct U16s, will be 8:
@[aligned]
struct U16s {
	data [3]u16
}
```
**When to Use**

- Only if the instances of your types, will be used in performance critical sections, or with
specialised machine instructions, that do require a specific alignment to work.

**When to Avoid**

- On CPU architectures, that do not support unaligned memory access. If you are not working on
performance critical algorithms, you do not really need it, since the proper minimum alignment
is CPU specific, and the compiler already usually will choose a good default for you.

> [!NOTE]
> You can leave out the alignment factor, i.e. use just `@[aligned]`, in which case the compiler
> will align a type to the maximum useful alignment for the target machine you are compiling for,
> i.e. the alignment will be the largest alignment which is ever used for any data type on the
> target machine. Doing this can often make copy operations more efficient, because the compiler
> can choose whatever instructions copy the biggest chunks of memory, when performing copies to or
> from the variables which have types that you have aligned this way.

See also ["What Every Programmer Should Know About Memory", by Ulrich Drepper](https://people.freebsd.org/~lstewart/articles/cpumemory.pdf) .

#### `@[minify]`

The `@[minify]` attribute can be added to a struct, allowing the compiler to reorder the fields in
a way that minimizes internal gaps while maintaining alignment. Using the `@[minify]` attribute may
cause issues with binary serialization or reflection. Be mindful of these potential side effects
when using this attribute.

**When to Use**

- When you want to minimize memory usage and you're not using binary serialization or reflection.

**When to Avoid**

- When using binary serialization or reflection, as it may cause unexpected behavior.

#### `_likely_/_unlikely_`

`if _likely_(bool expression) {` - hints to the C compiler, that the passed boolean expression is
very likely to be true, so it can generate assembly code, with less chance of branch misprediction.
In the JS backend, that does nothing.

`if _unlikely_(bool expression) {` is similar to `_likely_(x)`, but it hints that the boolean
expression is highly improbable. In the JS backend, that does nothing.

**When to Use**

- In conditional statements where one branch is clearly more frequently executed than the other.

**When to Avoid**

- When the prediction can be wrong, as it might cause a performance penalty due to branch
misprediction.

**When to Use**

- For production builds where you want to reduce the executable size and improve runtime
performance.

**When to Avoid**

- Where it doesn't work for you.

#### `-fast-math`

This flag enables optimizations that disregard strict compliance with the IEEE standard for
floating-point arithmetic. While this could lead to faster code, it may produce incorrect or
less accurate mathematical results.

The full specter of math operations that `-fast-math` affects can be found
[here](https://clang.llvm.org/docs/UsersManual.html#cmdoption-ffast-math).

**When to Use**

- In applications where performance is more critical than precision, like certain graphics
rendering tasks.

**When to Avoid**

- In applications requiring strict mathematical accuracy, such as scientific simulations or
financial calculations.

#### `-d no_segfault_handler`

Using this flag omits the segfault handler, reducing the executable size and potentially improving
compile time. However, in the case of a segmentation fault, the output will not contain stack trace
information, making debugging more challenging.

**When to Use**

- In small, well-tested utilities where a stack trace is not essential for debugging.

**When to Avoid**

- In large-scale, complex applications where robust debugging is required.

#### `-cflags -march=native`

This flag directs the C compiler to generate instructions optimized for the host CPU. This can
improve performance but will produce an executable incompatible with other/older CPUs.

**When to Use**

- When the software is intended to run only on the build machine or in a controlled environment
with identical hardware.

**When to Avoid**

- When distributing the software to users with potentially older CPUs.

#### `-compress`

This flag executes `upx` to compress the resultant executable, reducing its size by around 50%-70%.
The executable will be uncompressed at runtime, so it will take a bit more time to start.
It will also take extra RAM initially, as the compressed version of the app will be loaded into
memory, and then expanded to another chunk of memory.
Debugging such an application can be a bit harder, if you do not account for it.
Some antivirus programs also use heuristics, that trigger more often for compressed applications.

**When to Use**

- For really tiny environments, where the size of the executable on the file system,
or when deploying is important (docker containers, rescue disks etc).

**When to Avoid**

- When you need to debug the application
- When the app's startup time is extremely important (where 1-2ms can be meaningful for you)
- When you can not afford to allocate more memory during application startup
- When you are deploying an app to users with antivirus software that could misidentify your
app as malicious, just because it decompresses its code at runtime.

#### PGO (Profile-Guided Optimization)

PGO allows the compiler to optimize code based on its behavior during sample runs. This can improve
performance and reduce the size of the output executable, but it adds complexity to the build
process.

**When to Use**

- For performance-critical applications where the added build complexity is justifiable.

**When to Avoid**

- For small, short-lived, or rapidly-changing projects where the added build complexity isn't
justified.

**PGO with Clang**

This is an example bash script you can use to optimize your CLI V program without user interactions.
In most cases, you will need to change this script to make it suitable for your particular program.

```bash
#!/usr/bin/env bash

# Get the full path to the current directory
CUR_DIR=$(pwd)

# Remove existing PGO data
rm -f *.profraw
rm -f default.profdata

# Initial build with PGO instrumentation
v -cc clang -prod -cflags -fprofile-generate -o pgo_gen .

# Run the instrumented executable 10 times
for i in {1..10}; do
    ./pgo_gen
done

# Merge the collected data
llvm-profdata merge -o default.profdata *.profraw

# Compile the optimized version using the PGO data
v -cc clang -prod -cflags "-fprofile-use=${CUR_DIR}/default.profdata" -o optimized_program .

# Remove PGO data and instrumented executable
rm *.profraw
rm pgo_gen
```

## Atomics

V has no special support for atomics, yet, nevertheless it's possible to treat variables as atomics
by [calling C](#v-and-c) functions from V. The standard C11 atomic functions like `atomic_store()`
are usually defined with the help of macros and C compiler magic to provide a kind of
*overloaded C functions*.
Since V does not support overloading functions by intention there are wrapper functions defined in
C headers named `atomic.h` that are part of the V compiler infrastructure.

There are dedicated wrappers for all unsigned integer types and for pointers.
(`u8` is not fully supported on Windows) &ndash; the function names include the type name
as suffix. e.g. `C.atomic_load_ptr()` or `C.atomic_fetch_add_u64()`.

To use these functions the C header for the used OS has to be included and the functions
that are intended to be used have to be declared. Example:

```v globals
$if windows {
	#include "@VEXEROOT/thirdparty/stdatomic/win/atomic.h"
} $else {
	#include "@VEXEROOT/thirdparty/stdatomic/nix/atomic.h"
}

// declare functions we want to use - V does not parse the C header
fn C.atomic_store_u32(&u32, u32)
fn C.atomic_load_u32(&u32) u32
fn C.atomic_compare_exchange_weak_u32(&u32, &u32, u32) bool
fn C.atomic_compare_exchange_strong_u32(&u32, &u32, u32) bool

const num_iterations = 10000000

// see section "Global Variables" below
__global (
	atom u32 // ordinary variable but used as atomic
)

fn change() int {
	mut races_won_by_change := 0
	for {
		mut cmp := u32(17) // addressable value to compare with and to store the found value
		// atomic version of `if atom == 17 { atom = 23 races_won_by_change++ } else { cmp = atom }`
		if C.atomic_compare_exchange_strong_u32(&atom, &cmp, 23) {
			races_won_by_change++
		} else {
			if cmp == 31 {
				break
			}
			cmp = 17 // re-assign because overwritten with value of atom
		}
	}
	return races_won_by_change
}

fn main() {
	C.atomic_store_u32(&atom, 17)
	t := spawn change()
	mut races_won_by_main := 0
	mut cmp17 := u32(17)
	mut cmp23 := u32(23)
	for i in 0 .. num_iterations {
		// atomic version of `if atom == 17 { atom = 23 races_won_by_main++ }`
		if C.atomic_compare_exchange_strong_u32(&atom, &cmp17, 23) {
			races_won_by_main++
		} else {
			cmp17 = 17
		}
		desir := if i == num_iterations - 1 { u32(31) } else { u32(17) }
		// atomic version of `for atom != 23 {} atom = desir`
		for !C.atomic_compare_exchange_weak_u32(&atom, &cmp23, desir) {
			cmp23 = 23
		}
	}
	races_won_by_change := t.wait()
	atom_new := C.atomic_load_u32(&atom)
	println('atom: ${atom_new}, #exchanges: ${races_won_by_main + races_won_by_change}')
	// prints `atom: 31, #exchanges: 10000000`)
	println('races won by\n- `main()`: ${races_won_by_main}\n- `change()`: ${races_won_by_change}')
}
```

In this example both `main()` and the spawned thread `change()` try to replace a value of `17`
in the global `atom` with a value of `23`. The replacement in the opposite direction is
done exactly 10000000 times. The last replacement will be with `31` which makes the spawned
thread finish.

It is not predictable how many replacements occur in which thread, but the sum will always
be 10000000. (With the non-atomic commands from the comments the value will be higher or the program
will hang &ndash; dependent on the compiler optimization used.)

## Global Variables

By default V does not allow global variables. However, in low level applications they have their
place so their usage can be enabled with the compiler flag `-enable-globals`.
Declarations of global variables must be surrounded with a `__global ( ... )`
specification &ndash; as in the example [above](#atomics).

An initializer for global variables must be explicitly converted to the
desired target type. If no initializer is given a default initialization is done.
Some objects like semaphores and mutexes require an explicit initialization *in place*, i.e.
not with a value returned from a function call but with a method call by reference.
A separate `init()` function can be used for this purpose &ndash; it will be called before `main()`:

```v globals
import sync

__global (
	sem   sync.Semaphore // needs initialization in `init()`
	mtx   sync.RwMutex // needs initialization in `init()`
	f1    = f64(34.0625) // explicitly initialized
	shmap shared map[string]f64 // initialized as empty `shared` map
	f2    f64 // initialized to `0.0`
)

fn init() {
	sem.init(0)
	mtx.init()
}
```

Be aware that in multi threaded applications the access to global variables is subject
to race conditions. There are several approaches to deal with these:

- use `shared` types for the variable declarations and use `lock` blocks for access.
  This is most appropriate for larger objects like structs, arrays or maps.
- handle primitive data types as "atomics" using special C-functions (see [above](#atomics)).
- use explicit synchronization primitives like mutexes to control access. The compiler
  cannot really help in this case, so you have to know what you are doing.
- don't care &ndash; this approach is possible but makes only sense if the exact values
  of global variables do not really matter. An example can be found in the `rand` module
  where global variables are used to generate (non cryptographic) pseudo random numbers.
  In this case data races lead to random numbers in different threads becoming somewhat
  correlated, which is acceptable considering the performance penalty that using
  synchronization primitives would represent.

## Static Variables

V also supports *static variables*, which are like *global variables*, but
available only *inside* a single unsafe function (you can look at them as
namespaced globals).

Note: their use is discouraged too, for reasons similar to why globals
are discouraged. The feature is supported to enable translating existing
low level C code into V code, using `v translate`.

Note: the function in which you use a static variable, has to be marked
with @[unsafe]. Also unlike using globals, using static variables, do not
require you to pass the flag `-enable-globals`, because they can only be
read/changed inside a single function, which has full control over the
state stored in them.

Here is a small example of how static variables can be used:
```v
@[unsafe]
fn counter() int {
	mut static x := 42
	// Note: x is initialised to 42, just _once_.
	x++
	return x
}

fn f() int {
	return unsafe { counter() }
}

println(f()) // prints 43
println(f()) // prints 44
println(f()) // prints 45
```

## Cross compilation

Cross compilation is supported for Windows, Linux and FreeBSD.

To cross compile your project simply run:

```shell
v -os windows .
```

or

```shell
v -os linux .
```

or

```shell
v -os freebsd .
```

> [!NOTE]
> Cross-compiling a Windows binary on a Linux machine requires the GNU C compiler for
> MinGW-w64 (targeting Win64) to first be installed.

For Ubuntu/Debian based distributions:

```shell
sudo apt install gcc-mingw-w64-x86-64
```

For Arch based distributions:

```shell
sudo pacman -S mingw-w64-gcc
```

(Cross compiling for macOS is temporarily not possible.)

If you don't have any C dependencies, that's all you need to do. This works even
when compiling GUI apps using the `ui` module or graphical apps using `gg`.

You will need to install Clang, LLD linker, and download a zip file with
libraries and include files for Windows and Linux. V will provide you with a link.

## Debugging

### C Backend binaries (Default)

To debug issues in the generated binary (flag: `-b c`), you can pass these flags:

- `-g` - produces a less optimized executable with more debug information in it.
  V will enforce line numbers from the .v files in the stacktraces, that the
  executable will produce on panic. It is usually better to pass -g, unless
  you are writing low level code, in which case use the next option `-cg`.
- `-cg` - produces a less optimized executable with more debug information in it.
  The executable will use C source line numbers in this case. It is frequently
  used in combination with `-keepc`, so that you can inspect the generated
  C program in case of panic, or so that your debugger (`gdb`, `lldb` etc.)
  can show you the generated C source code.
- `-showcc` - prints the C command that is used to build the program.
- `-show-c-output` - prints the output, that your C compiler produced
  while compiling your program.
- `-keepc` - do not delete the generated C source code file after a successful
  compilation. Also keep using the same file path, so it is more stable,
  and easier to keep opened in an editor/IDE.

For best debugging experience if you are writing a low level wrapper for an existing
C library, you can pass several of these flags at the same time:
`v -keepc -cg -showcc yourprogram.v`, then just run your debugger (gdb/lldb) or IDE
on the produced executable `yourprogram`.

If you just want to inspect the generated C code,
without further compilation, you can also use the `-o` flag (e.g. `-o file.c`).
This will make V produce the `file.c` then stop.

If you want to see the generated C source code for *just* a single C function,
for example `main`, you can use: `-printfn main -o file.c`.

To debug the V executable itself you need to compile from src with `./v -g -o v cmd/v`.

You can debug tests with for example `v -g -keepc prog_test.v`. The `-keepc` flag is needed,
so that the executable is not deleted, after it was created and ran.

To see a detailed list of all flags that V supports,
use `v help`, `v help build` and `v help build-c`.

**Commandline Debugging**

1. compile your binary with debugging info `v -g hello.v`
2. debug with [lldb](https://lldb.llvm.org) or [GDB](https://www.gnu.org/software/gdb/)
   e.g. `lldb hello`

[Troubleshooting (debugging) executables created with V in GDB](https://github.com/vlang/v/wiki/Troubleshooting-(debugging)-executables-created-with-V-in-GDB)

**Visual debugging Setup:**

* [Visual Studio Code](vscode.md)

### Native Backend binaries

Currently there is no debugging support for binaries, created by the
native backend (flag: `-b native`).

### Javascript Backend

To debug the generated Javascript output you can activate source maps:
`v -b js -sourcemap hello.v -o hello.js`

For all supported options check the latest help:
`v help build-js`

## V and C

The basic mapping between C and V types is described in
[C and V Type Interoperability](https://github.com/vlang/v/blob/master/doc/c_and_v_type_interoperability.md).

### Calling C from V

V currently does not have a parser for C code. That means that even
though it allows you to `#include` existing C header and source files,
it will not know anything about the declarations in them. The `#include`
statement will only appear in the generated C code, to be used by the
C compiler backend itself.

**Example of #include**
```v oksyntax
#include <stdio.h>
```
After this statement, V will *not* know anything about the functions and
structs declared in `stdio.h`, but if you try to compile the .v file,
it will add the include in the generated C code, so that if that header file
is missing, you will get a C error (you will not in this specific case, if you
have a proper C compiler setup, since `<stdio.h>` is part of the
standard C library).

To overcome that limitation (that V does not have a C parser), V needs you to
redeclare the C functions and structs, on the V side, in your `.c.v` files.
Note that such redeclarations only need to have enough details about the
functions/structs that you want to use.
Note also that they *do not have* to be complete, unlike the ones in the .h files.


**C. struct redeclarations**
For example, if a struct has 3 fields on the C side, but you want to only
refer to 1 of them, you can declare it like this:

**Example of C struct redeclaration**
```v oksyntax
struct C.NameOfTheStruct {
	a_field int
}
```
Another feature, that is very frequently needed for C interoperability,
is the `@[typedef]` attribute. It is used for marking `C.` structs,
that are defined with `typedef struct SomeName { ..... } TypeName;` in the C headers.

For that case, you will have to write something like this in your .c.v file:
```v oksyntax
@[typedef]
pub struct C.TypeName {
}
```
Note that the name of the `C.` struct in V, is the one *after* the `struct SomeName {...}`.

**C. function redeclarations**
The situation is similar for `C.` functions. If you are going to call just 1 function in a
library, but its .h header declares dozens of them, you will only need to declare that single
function, for example:

**Example of C function redeclaration**
```v oksyntax
fn C.name_of_the_C_function(param1 int, const_param2 &char, param3 f32) f64
```
... and then later, you will be able to call the same way you would V function:
```v oksyntax
f := C.name_of_the_C_function(123, c'here is some C style string', 1.23)
dump(f)
```

**Example of using a C function from stdio, by redeclaring it on the V side**
```v
#include <stdio.h>

// int dprintf(int fd, const char *format, ...)
fn C.dprintf(fd int, const_format &char, ...voidptr) int

value := 12345
x := C.dprintf(0, c'Hello world, value: %d\n', value)
dump(x)
```

If your C backend compiler is properly setup, you should see something like this, when you try
to run it:
```console
#0 10:42:32 /v/examples> v run a.v
Hello world, value: 12345
[a.v:8] x: 26
#0 10:42:33 /v/examples>
```

Note, that the C function redeclarations look very similar to the V ones, with some differences:
1) They lack a body (they are defined on the C side) .
2) Their names start with `C.` .
3) Their names can have capital letters (unlike V ones, that are required to use snake_case) .

Note also the second parameter `const char *format`, which was redeclared as `const_format &char` .
The `const_` prefix in that redeclaration may seem arbitrary, but it is important, if you want
to compile your code with `-cstrict` or thirdparty C static analysis tools. V currently does not
have another way to express that this parameter is a const (this will probably change in V 1.0).

For some C functions, that use variadics (`...`) as parameters, V supports a special syntax for
the parameters - `...voidptr`, that is not available for ordinary V functions (V's variadics are
*required* to have the same exact type). Usually those are functions of the printf/scanf family
i.e for `printf`, `fprintf`, `scanf`, `sscanf`, etc, and other formatting/parsing/logging
functions.

**Example**

```v
#flag freebsd -I/usr/local/include -L/usr/local/lib
#flag -lsqlite3
#include "sqlite3.h"
// See also the example from https://www.sqlite.org/quickstart.html
pub struct C.sqlite3 {
}

pub struct C.sqlite3_stmt {
}

type FnSqlite3Callback = fn (voidptr, int, &&char, &&char) int

fn C.sqlite3_open(&char, &&C.sqlite3) int

fn C.sqlite3_close(&C.sqlite3) int

fn C.sqlite3_column_int(stmt &C.sqlite3_stmt, n int) int

// ... you can also just define the type of parameter and leave out the C. prefix

fn C.sqlite3_prepare_v2(&C.sqlite3, &char, int, &&C.sqlite3_stmt, &&char) int

fn C.sqlite3_step(&C.sqlite3_stmt)

fn C.sqlite3_finalize(&C.sqlite3_stmt)

fn C.sqlite3_exec(db &C.sqlite3, sql &char, cb FnSqlite3Callback, cb_arg voidptr, emsg &&char) int

fn C.sqlite3_free(voidptr)

fn my_callback(arg voidptr, howmany int, cvalues &&char, cnames &&char) int {
	unsafe {
		for i in 0 .. howmany {
			print('| ${cstring_to_vstring(cnames[i])}: ${cstring_to_vstring(cvalues[i]):20} ')
		}
	}
	println('|')
	return 0
}

fn main() {
	db := &C.sqlite3(unsafe { nil }) // this means `sqlite3* db = 0`
	// passing a string literal to a C function call results in a C string, not a V string
	C.sqlite3_open(c'users.db', &db)
	// C.sqlite3_open(db_path.str, &db)
	query := 'select count(*) from users'
	stmt := &C.sqlite3_stmt(unsafe { nil })
	// Note: You can also use the `.str` field of a V string,
	// to get its C style zero terminated representation
	C.sqlite3_prepare_v2(db, &char(query.str), -1, &stmt, 0)
	C.sqlite3_step(stmt)
	nr_users := C.sqlite3_column_int(stmt, 0)
	C.sqlite3_finalize(stmt)
	println('There are ${nr_users} users in the database.')

	error_msg := &char(unsafe { nil })
	query_all_users := 'select * from users'
	rc := C.sqlite3_exec(db, &char(query_all_users.str), my_callback, voidptr(7), &error_msg)
	if rc != C.SQLITE_OK {
		eprintln(unsafe { cstring_to_vstring(error_msg) })
		C.sqlite3_free(error_msg)
	}
	C.sqlite3_close(db)
}
```

### Calling V from C

Since V can compile to C, calling V code from C is very easy, once you know how.

Use `v -o file.c your_file.v` to generate a C file, corresponding to the V code.

More details in [call_v_from_c example](../examples/call_v_from_c).

### Passing C compilation flags

Add `#flag` directives to the top of your V files to provide C compilation flags like:

- `-I` for adding C include files search paths
- `-l` for adding C library names that you want to get linked
- `-L` for adding C library files search paths
- `-D` for setting compile time variables

You can (optionally) use different flags for different targets.
Currently the `linux`, `darwin` , `freebsd`, and `windows` flags are supported.

> [!NOTE]
> Each flag must go on its own line (for now)

```v oksyntax
#flag linux -lsdl2
#flag linux -Ivig
#flag linux -DCIMGUI_DEFINE_ENUMS_AND_STRUCTS=1
#flag linux -DIMGUI_DISABLE_OBSOLETE_FUNCTIONS=1
#flag linux -DIMGUI_IMPL_API=
```

In the console build command, you can use:

* `-cc` to change the default C backend compiler.
* `-cflags` to pass custom flags to the backend C compiler (passed before other C options).
* `-ldflags` to pass custom flags to the backend C linker (passed after every other C option).
* For example: `-cc gcc-9 -cflags -fsanitize=thread`.

You can define a `VFLAGS` environment variable in your terminal to store your `-cc`
and `-cflags` settings, rather than including them in the build command each time.

### #pkgconfig

Add `#pkgconfig` directives to tell the compiler which modules should be used for compiling
and linking using the pkg-config files provided by the respective dependencies.

As long as backticks can't be used in `#flag` and spawning processes is not desirable for security
and portability reasons, V uses its own pkgconfig library that is compatible with the standard
freedesktop one.

If no flags are passed it will add `--cflags` and `--libs` to pkgconfig (not to V).
In other words, both lines below do the same:

```v oksyntax
#pkgconfig r_core
#pkgconfig --cflags --libs r_core
```

The `.pc` files are looked up into a hardcoded list of default pkg-config paths, the user can add
extra paths by using the `PKG_CONFIG_PATH` environment variable. Multiple modules can be passed.

To check the existence of a pkg-config use `$pkgconfig('pkg')` as a compile time "if" condition to
check if a pkg-config exists. If it exists the branch will be created. Use `$else` or `$else $if`
to handle other cases.

```v ignore
$if $pkgconfig('mysqlclient') {
	#pkgconfig mysqlclient
} $else $if $pkgconfig('mariadb') {
	#pkgconfig mariadb
}
```

### Including C code

You can also include C code directly in your V module.
For example, let's say that your C code is located in a folder named 'c' inside your module folder.
Then:

* Put a v.mod file inside the toplevel folder of your module (if you
  created your module with `v new` you already have v.mod file). For example:

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
#flag -I @VMODROOT/c
#flag @VMODROOT/c/implementation.o
#include "header.h"
```

> [!NOTE]
> @VMODROOT will be replaced by V with the *nearest parent folder,
> where there is a v.mod file*.
> Any .v file beside or below the folder where the v.mod file is,
> can use `#flag @VMODROOT/abc` to refer to this folder.
> The @VMODROOT folder is also *prepended* to the module lookup path,
> so you can *import* other modules under your @VMODROOT, by just naming them.

The instructions above will make V look for an compiled .o file in
your module `folder/c/implementation.o`.
If V finds it, the .o file will get linked to the main executable, that used the module.
If it does not find it, V assumes that there is a `@VMODROOT/c/implementation.c` file,
and tries to compile it to a .o file, then will use that.

This allows you to have C code, that is contained in a V module, so that its distribution is easier.
You can see a complete minimal example for using C code in a V wrapper module here:
[project_with_c_code](https://github.com/vlang/v/tree/master/vlib/v/tests/project_with_c_code).
Another example, demonstrating passing structs from C to V and back again:
[interoperate between C to V to C](https://github.com/vlang/v/tree/master/vlib/v/tests/project_with_c_code_2).

### C types

Ordinary zero terminated C strings can be converted to V strings with
`unsafe { &char(cstring).vstring() }` or if you know their length already with
`unsafe { &char(cstring).vstring_with_len(len) }`.

> [!NOTE]
> The `.vstring()` and `.vstring_with_len()` methods do NOT create a copy of the `cstring`,
> so you should NOT free it after calling the method `.vstring()`.
> If you need to make a copy of the C string (some libc APIs like `getenv` pretty much require that,
> since they return pointers to internal libc memory), you can use `cstring_to_vstring(cstring)`.

On Windows, C APIs often return so called `wide` strings (UTF-16 encoding).
These can be converted to V strings with `string_from_wide(&u16(cwidestring))` .

V has these types for easier interoperability with C:

- `voidptr` for C's `void*`,
- `&u8` for C's `byte*` and
- `&char` for C's `char*`.
- `&&char` for C's `char**`

To cast a `voidptr` to a V reference, use `user := &User(user_void_ptr)`.

`voidptr` can also be dereferenced into a V struct through casting: `user := User(user_void_ptr)`.

[an example of a module that calls C code from V](https://github.com/vlang/v/blob/master/vlib/v/tests/project_with_c_code/mod1/wrapper.c.v)

### C Declarations

C identifiers are accessed with the `C` prefix similarly to how module-specific
identifiers are accessed. Functions must be redeclared in V before they can be used.
Any C types may be used behind the `C` prefix, but types must be redeclared in V in
order to access type members.

To redeclare complex types, such as in the following C code:

```c
struct SomeCStruct {
	uint8_t implTraits;
	uint16_t memPoolData;
	union {
		struct {
			void* data;
			size_t size;
		};

		DataView view;
	};
};
```

members of sub-data-structures may be directly declared in the containing struct as below:

```v
pub struct C.SomeCStruct {
	implTraits  u8
	memPoolData u16
	// These members are part of sub data structures that can't currently be represented in V.
	// Declaring them directly like this is sufficient for access.
	// union {
	// struct {
	data voidptr
	size usize
	// }
	view C.DataView
	// }
}
```

The existence of the data members is made known to V, and they may be used without
re-creating the original structure exactly.

Alternatively, you may [embed](#embedded-structs) the sub-data-structures to maintain
a parallel code structure.

### Export to shared library

By default all V functions have the following naming scheme in C: `[module name]__[fn_name]`.

For example, `fn foo() {}` in module `bar` will result in `bar__foo()`.

To use a custom export name, use the `@[export]` attribute:

```
@[export: 'my_custom_c_name']
fn foo() {
}
```

### Translating C to V

V can translate your C code to human readable V code, and generating V wrappers
on top of C libraries.

C2V currently uses Clang's AST to generate V, so to translate a C file to V
you need to have Clang installed on your machine.

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
v translate wrapper c_code/libsodium/src/libsodium
```

This will generate a directory `libsodium` with a V module.

Example of a C2V generated libsodium wrapper:

https://github.com/vlang/libsodium

<br>

When should you translate C code and when should you simply call C code from V?

If you have well-written, well-tested C code,
then of course you can always simply call this C code from V.

Translating it to V gives you several advantages:

- If you plan to develop that code base, you now have everything in one language,
  which is much safer and easier to develop in than C.
- Cross-compilation becomes a lot easier. You don't have to worry about it at all.
- No more build flags and include files either.

### Working around C issues

In some cases, C interop can be extremely difficult.
One of these such cases is when headers conflict with each other.
For example, V needs to include the Windows header libraries in order for your V binaries to work
seamlessly across all platforms.

However, since the Windows header libraries use extremely generic names such as `Rectangle`,
this will cause a conflict if you wish to use C code that also has a name defined as `Rectangle`.

For very specific cases like this, V has `#preinclude` and `#postinclude` directives.

These directives allow things to be configured *before* V adds in its built in libraries,
and *after* all of the V code generation has completed (and thus all of the prototypes,
declarations and definitions are already present).

Example usage:
```v ignore
// This will include before built in libraries are used.
#preinclude "pre_include.h"

// This will include after built in libraries are used.
#include "include.h"

// This will include after all of the V code generation is complete,
// including the one for the main function of the project
#postinclude "post_include.h"
```

An example of what might be included in `pre_include.h`
can be [found here](https://github.com/irishgreencitrus/raylib.v/blob/main/include/pre.h)

The `#postinclude` directive on the other hand is useful for allowing the integration
of frameworks like SDL3 or Sokol, that insist on having callbacks in your code, instead
of behaving like ordinary libraries, and allowing you to decide when to call them.

NOTE: these are advanced features, and will not be necessary outside of very specific cases
with C interop. Other than those, using them could cause more issues than it solves.

Consider using them as a last resort!

## Other V Features

### Inline assembly

<!-- ignore because it doesn't pass fmt test (why?) -->

```v ignore
a := 100
b := 20
mut c := 0
asm amd64 {
    mov eax, a
    add eax, b
    mov c, eax
    ; =r (c) as c // output
    ; r (a) as a // input
      r (b) as b
}
println('a: ${a}') // 100
println('b: ${b}') // 20
println('c: ${c}') // 120
```

For more examples, see
[vlib/v/slow_tests/assembly/asm_test.amd64.v](https://github.com/vlang/v/tree/master/vlib/v/slow_tests/assembly/asm_test.amd64.v)

### Hot code reloading

```v live
module main

import time

@[live]
fn print_message() {
	println('Hello! Modify this message while the program is running.')
}

fn main() {
	for {
		print_message()
		time.sleep(500 * time.millisecond)
	}
}
```

Build this example with `v -live message.v`.

You can also run this example with `v -live run message.v`.
Make sure that in command you use a path to a V's file,
**not** a path to a folder (like `v -live run .`) -
in that case you need to modify content of a folder (add new file, for example),
because changes in *message.v* will have no effect.

Functions that you want to be reloaded must have `@[live]` attribute
before their definition.

Right now it's not possible to modify types while the program is running.

More examples, including a graphical application:
[github.com/vlang/v/tree/master/examples/hot_reload](https://github.com/vlang/v/tree/master/examples/hot_reload).

#### About keeping states in hot reloading functions with v -live run
V's hot code reloading relies on marking the functions that you want to reload with `@[live]`,
then compiling a shared library of these `@[live]` functions, and then
your v program loads that shared library at runtime.

V (with the -live option) starts a new thread, that monitors the source files for changes,
and when it detects modifications, it recompiles the shared library, and reloads it at runtime,
so that new calls to those @[live] functions will be made to the newly loaded library.

It keeps all the accumulated state (from locals outside the @[live] functions,
from heap variables and from globals), allowing to tweak the code in the merged functions quickly.

When there are more substantial changes (to data structures, or to functions that were not marked),
you will have to restart the running app manually.

### Cross-platform shell scripts in V

V can be used as an alternative to Bash to write deployment scripts, build scripts, etc.

The advantage of using V for this, is the simplicity and predictability of the language, and
cross-platform support. "V scripts" run on Unix-like systems, as well as on Windows.

To use V's script mode, save your source file with the `.vsh` file extension.
It will make all functions in the `os` module global (so that you can use `mkdir()` instead
of `os.mkdir()`, for example).

V also knows to compile & run `.vsh` files immediately, so you do not need a separate
step to compile them. V will also recompile an executable, produced by a `.vsh` file,
*only when it is older than the .vsh source file*, i.e. runs after the first one, will
be faster, since there is no need for a re-compilation of a script, that has not been changed.

An example `deploy.vsh`:

```v oksyntax
#!/usr/bin/env -S v

// Note: The shebang line above, associates the .vsh file to V on Unix-like systems,
// so it can be run just by specifying the path to the .vsh file, once it's made
// executable, using `chmod +x deploy.vsh`, i.e. after that chmod command, you can
// run the .vsh script, by just typing its name/path like this: `./deploy.vsh`

// print command then execute it
fn sh(cmd string) {
	println('❯ ${cmd}')
	print(execute_or_exit(cmd).output)
}

// Remove if build/ exits, ignore any errors if it doesn't
rmdir_all('build') or {}

// Create build/, never fails as build/ does not exist
mkdir('build')!

// Move *.v files to build/
result := execute('mv *.v build/')
if result.exit_code != 0 {
	println(result.output)
}

sh('ls')

// Similar to:
// files := ls('.')!
// mut count := 0
// if files.len > 0 {
//     for file in files {
//         if file.ends_with('.v') {
//              mv(file, 'build/') or {
//                  println('err: ${err}')
//                  return
//              }
//         }
//         count++
//     }
// }
// if count == 0 {
//     println('No files')
// }
```

Now you can either compile this like a normal V program and get an executable you can deploy and run
anywhere:
`v -skip-running deploy.vsh && ./deploy`

Or run it like a traditional Bash script:
`v run deploy.vsh` (or simply just `v deploy.vsh`)

On Unix-like platforms, the file can be run directly after making it executable using `chmod +x`:
`./deploy.vsh`

### Vsh scripts with no extension

Whilst V does normally not allow vsh scripts without the designated file extension, there is a way
to circumvent this rule and have a file with a fully custom name and shebang. Whilst this feature
exists it is only recommended for specific usecases like scripts that will be put in the path and
should **not** be used for things like build or deploy scripts. To access this feature start the
file with `#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp` where `tmp` is the prefix for
the built executable. This will run in crun mode so it will only rebuild if changes to the script
were made and keep the binary as `tmp.<scriptfilename>`. **Caution**: if this filename already
exists the file will be overridden. If you want to rebuild each time and not keep this binary
instead use `#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp run`.

Note: there is a small shell script `cmd/tools/vrun`, that can be useful for systems, that have an
env program (`/usr/bin/env`), that still does not support an `-S` option (like BusyBox).
See https://github.com/vlang/v/blob/master/cmd/tools/vrun for more details.

# Appendices

## Appendix I: Keywords

V has 45 reserved keywords (3 are literals):

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
enum
false
fn
for
go
goto
if
implements
import
in
interface
is
isreftype
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
spawn
static
struct
true
type
typeof
union
unsafe
volatile
__global
__offsetof
```

See also [V Types](#v-types).

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
>>>  unsigned right shift   integer >> unsigned integer


Precedence    Operator
    5            *  /  %  <<  >> >>> &
    4            +  -  |  ^
    3            ==  !=  <  <=  >  >=
    2            &&
    1            ||


Assignment Operators
+=   -=   *=   /=   %=
&=   |=   ^=
>>=  <<=  >>>=
&&= ||=
```

Note: in V, `assert -10 % 7 == -3` passes. In programming, the sign of the remainder
depends upon the signs of divisor and dividend.

## Other online resources

### [V contributing guide](https://github.com/vlang/v/blob/master/CONTRIBUTING.md)

V would be much less, than what it is today, without the help of all
its contributors. If you like and want to help the V project succeed,
please read that document, choose a task, and dive in!

### [V language documentation](https://docs.vlang.io/introduction.html)
The site has the same information as this document, but split to pages,
for easier reading on mobile devices. Updated automatically on each
commit to the main repository.

### [V standard module documentation](https://modules.vlang.io/)
The site has the documentation of all the modules in V's standard
library (vlib). Updated automatically on each commit to the main
repository.

### [V online playground](https://play.vlang.io/)
The site allows you to enter and edit small V programs, then compile
and run them. Updated automatically on each commit to the main
repository. Use it, to test your ideas, when you do not have access
to a computer or an Android phone, where V has been already installed.

### [Awesome V](https://github.com/vlang/awesome-v)
When you make a cool new project or a library, you can submit it to that
list too. You can also use the list, for ideas about new projects to do
with V.

### [The V language Discord](https://discord.gg/vlang)
This is the place to be, to discuss the V language, learn about latest
developments, quickly get help with issues, witness/participate in
~~epic flame wars~~ constructive criticism exchanges and design decisions.
Join it, and learn more about languages, games, editors, people, Klingons,
Conway's law and the universe.
