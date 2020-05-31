# V Documentation

## Introduction

V is a statically typed compiled programming language designed for building maintainable software.

It's similar to Go and its design has also been influenced by Oberon, Rust, Swift, and Python.

V is a very simple language. Going through this documentation will take you about half an hour,
and by the end of it you will have pretty much learned the entire language.

The language promotes writing simple and clear code with minimal abstraction.

Despite being simple, V gives the developer a lot of power. Anything you can do in other languages,
you can do in V.

## Table of Contents

<table>
	<tr>
		<td width=16%><a href='#hello-world'>1. Hello world</a></td>
		<td width=16%><a href='#comments'>2. Comments</a></td>
		<td width=16%><a href='#functions'>3. Functions</a></td>
		<td width=16%><a href='#variables'>4. Variables</a></td>
		<td width=16%><a href='#primitive-types'>5. Primitive types</a></td>
		<td width=16%><a href='#strings'>6. Strings</a></td>
	</tr>
	<tr>
		<td><a href='#imports'>7. Imports</a></td>
		<td><a href='#arrays'>Arrays</a></td>
		<td><a href='#maps'>Maps</a></td>
		<td><a href='#if'>If</a></td>
		<td><a href='#in-operator'>In Operator</a></td>
		<td><a href='#for-loop'>For loop</a></td>
		</tr>
	<tr>
		<td><a href='#match'>Match</a></td>
		<td><a href='#structs'>Structs</a></td>
		<td><a href='#short-struct-initialization-syntax'>Short struct init syntax</a></td>
		<td><a href='#access-modifiers'>Access modifiers</a></td>
		<td><a href='#methods'>Methods</a></td>
		<td><a href='#pure-functions-by-default'>Pure functions by default</a></td>
		</tr>
	<tr>
		<td><a href='#anonymous--high-order-functions'>Anonymous & high order fns</a></td>
		<td><a href='#references'>References</a></td>
		<td><a href='#constants'>Constants</a></td>
		<td><a href='#println'>println</a></td>
		<td><a href='#modules'>Modules</a></td>
		<td><a href='#interfaces'>Interfaces</a></td>
		</tr>
	<tr>
		<td><a href='#enums'>Enums</a></td>
		<td><a href='#sum-types'>Sum types</a></td>
		<td><a href='#optionresult-types-and-error-handling'>Option/Result & error handling</a></td>
		<td><a href='#generics'>Generics</a></td>
		<td><a href='#concurrency'>Concurrency</a></td>
		<td><a href='#decoding-json'>Decoding JSON</a></td>
		</tr>
	<tr>
		<td><a href='#testing'>Testing</a></td>
		<td><a href='#memory-management'>Memory managment</a></td>
		<td><a href='#defer'>Defer</a></td>
		<td><a href='#orm'>ORM</a></td>
		<td><a href='#vfmt'>vfmt</a></td>
		<td><a href='#writing-documentation'>Writing documentation</a></td>
		</tr>
	<tr>
		<td><a href='#calling-c-functions-from-v'>Calling C functions from V</a></td>
		<td><a href='#conditional-compilation'>Conditional compilation</a></td>
		<td><a href='#reflection-via-codegen'>Reflection via codegen</a></td>
		<td><a href='#limited-operator-overloading'>Limited operator overloading</a></td>
		<td><a href='#inline-assembly'>Inline assembly</a></td>
		<td><a href='#translating-cc-to-v'>Translating C/C++ to V</a></td>
		</tr>
	<tr>
		<td><a href='#hot-code-reloading'>Hot code reloading</a></td>
		<td><a href='#cross-compilation'>Cross compilation</a></td>
		<td><a href='#cross-platform-shell-scripts-in-v'>Cross-platform shell scripts in V</a></td>
		<td><a href='#appendix-i-keywords'>Appendix I: Keywords</a></td>
		<td><a href='#appendix-ii-operators'>Appendix II: Operators</a></td>
	</tr>
</table>



## Hello World

```v
fn main() {
    println('hello world')
}
```
Save that snippet into a file `hello.v` . Now do: `v run hello.v` .

> That is assuming you have symlinked your V with `v symlink`, as described 
[here](https://github.com/vlang/v/blob/master/README.md#symlinking).
If you have not yet, you have to type the path to V manually.

Congratulations - you just wrote your first V program, and executed it!

> You can compile a program without execution with `v hello.v`.
See `v help` for all supported commands.

In the above example, you can see that functions are declared with `fn`.
The return type goes after the function name. In this case `main` doesn't 
return anything, so the return type can be omitted.

As in many other languages (such as C, Go and Rust), `main` is an entry point.

`println` is one of the few built-in functions. It prints the value passed to it
to standard output.

`fn main()` declaration can be skipped in one file programs.
This is useful when writing small programs, "scripts", or just learning
the language. For brevity, `fn main()` will be skipped in this
tutorial.

This means that a "hello world" program can be as simple as

```v
println('hello world')
```

## Comments

```v
// This is a single line comment.

/* This is a multiline comment.
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

fn sub(x, y int) int {
    return x - y
}
```

Again, the type comes after the argument's name.

Just like in Go and C, functions cannot be overloaded.
This simplifies the code and improves maintainability and readability.

Functions can be used before their declaration:
`add` and `sub` are declared after `main`, but can still be called from `main`.
This is true for all declarations in V and eliminates the need of header files
or thinking about the order of files and declarations.

<p>&nbsp;</p>

```v
fn foo() (int, int) {
    return 2, 3
}

a, b := foo()
println(a) // 2
println(b) // 3
```

Functions can return multiple values.

<p>&nbsp;</p>


```v
pub fn public_function() {
}

fn private_function() {
}
```

Like constants and types, functions are private (not exported) by default.
To allow other modules to use them, prepend `pub`. The same applies
to constants and types.



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
To force a different type, use type conversion:
the expression `T(v)` converts the value `v` to the
type `T`.

Unlike most other languages, V only allows defining variables in functions.
Global (module level) variables are not allowed. There's no global state in V.

<p>&nbsp;</p>

```v
mut age := 20
println(age)
age = 21
println(age)
```

To change the value of the variable use `=`. In V, variables are
immutable by default. To be able to change the value of the variable, you have to declare it with `mut`.

Try compiling the program above after removing `mut` from the first line.

Note the (important) difference between `:=` and `=`
`:=` is used for declaring and initializing, `=` is used for assigning.

<p>&nbsp;</p>

```v
fn main() {
    age = 21
}
```

This code will not compile, because the variable `age` is not declared.
All variables need to be declared in V.

<p>&nbsp;</p>

```v
fn main() {
    age := 21
}
```

In development mode the compiler will warn you that you haven't used the variable (you'll get an "unused variable" warning).
In production mode (enabled by passing the `-prod` flag to v – `v -prod foo.v`) it will not compile at all (like in Go).

<p>&nbsp;</p>

```v
fn main() {
    a := 10
    if true {
        a := 20
    }
}
```

Unlike most languages, variable shadowing is not allowed. Declaring a variable with a name that is already used in a parent scope will cause a compilation error.

## Primitive types

```v
bool

string

i8    i16  int  i64      i128 (soon)
byte  u16  u32  u64      u128 (soon)

rune // represents a Unicode code point

f32 f64

any_int, any_float // internal intermediate types of number literals

byteptr // these two are mostly used for C interop
voidptr

any // similar to C's void* and Go's interface{}
```

Please note that unlike C and Go, `int` is always a 32 bit integer.

There is an exceptions to the rule that all operators
in V must have values of the same type on both sides. A small primitive type
on one side can be automatically promoted if it fits
completely into the data range of the type on the other side, i.e. when
the promotion does not result in any data loss.
These are the allowed possibilities:

```
   i8 → i16 → int → i64
            ↘     ↘
              f32 → f64
            ↗     ↗
 byte → u16 → u32 → u64 ⬎
      ↘     ↘     ↘      ptr
   i8 → i16 → int → i64 ⬏
```
An `int` value for example can be automatically promoted to `f64`
or `i64` but not to `f32` or `u32`. (`f32` would mean precission
loss for large values and `u32` would mean loss of the sign for
negative values).

## Strings

```v
name := 'Bob'
println('Hello, $name!')  // `$` is used for string interpolation
println(name.len)

bobby := name + 'by' // + is used to concatenate strings
println(bobby) // "Bobby"

println(bobby[1..3]) // "ob"
mut s := 'hello '
s += 'world' // `+=` is used to append to a string
println(s) // "hello world"
```

In V, a string is a read-only array of bytes. String data is encoded using UTF-8.

Strings are immutable.

Both single and double quotes can be used to denote strings. For consistency,
`vfmt` converts double quotes to single quotes unless the string contains a single quote character.

Interpolation syntax is pretty simple. It also works with fields:
`'age = $user.age'`. If you need more complex expressions, use `${}`: `'can register = ${user.age > 13}'`.

Format specifiers similar to those in C's `printf()` are also supported. `f`, `g`, `x`, etc. are optional
and specify the output format. The compiler takes care of the storage size, so there is no `hd` or `llu`.

```v
println('x = ${x:12.3f}')
println('${item:-20} ${n:20d}')
```

All operators in V must have values of the same type on both sides. This code will not compile if `age` is not a string (for example if `age` were an `int`):

```v
println('age = ' + age)
```

We have to either convert `age` to a `string`:

```v
println('age = ' + age.str())
```

or use string interpolation (preferred):

```v
println('age = $age')
```

To denote character literals, use `

```v
a := `a`
assert 'aloha!'[0] == `a`
```

For raw strings, prepend `r`. Raw strings are not escaped:

```v
s := r'hello\nworld'
println(s) // "hello\nworld"
```

## Imports

```v
import os

fn main() {
    name := os.input('Enter your name:')
    println('Hello, $name!')
}
```

Modules can be imported using keyword `import`. When using types, functions, and constants from other modules, the full path must be specified. In the example above, `name := input()` wouldn't work. That means that it's always clear from which module a function is called.

## Arrays

```v
mut nums := [1, 2, 3]
println(nums) // "[1, 2, 3]"
println(nums[1]) // "2"

nums << 4
println(nums) // "[1, 2, 3, 4]"

nums << [5, 6, 7]
println(nums) // "[1, 2, 3, 4, 5, 6, 7]"

mut names := ['John']
names << 'Peter'
names << 'Sam'
// names << 10  <-- This will not compile. `names` is an array of strings.
println(names.len) // "3"
println('Alex' in names) // "false"

names = [] // The array is now empty

// Declare an empty array:
users := []User{}

// We can also preallocate a certain amount of elements.
ids := []int{ len: 50, init: 0 } // This creates an array with 50 zeros
```

The type of an array is determined by the first element: `[1, 2, 3]` is an array of ints (`[]int`).

`['a', 'b']` is an array of strings (`[]string`).

V arrays are homogenous (all elements must have the same type). This means that code like `[1, 'a']` will not compile.

`<<` is an operator that appends a value to the end of the array.
It can also append an entire array.

`.len` field returns the length of the array. Note, that it's a read-only field,
and it can't be modified by the user. Exported fields are read-only by default in V.

`val in array` returns true if the array contains `val`.

All arrays can be easily printed with `println(arr)` and converted to a string
with `s := arr.str()`.

Arrays can be efficiently filtered and mapped with the `.filter()` and
`.map()` methods:

```v
nums := [1, 2, 3, 4, 5, 6]
even := nums.filter(it % 2 == 0)
println(even) // [2, 4, 6]

words := ['hello', 'world']
upper := words.map(it.to_upper())
println(upper) // ['HELLO', 'WORLD']
```

`it` is a builtin variable which refers to element currently being processed in filter/map methods.

## Maps

```v
mut m := map[string]int // Only maps with string keys are allowed for now
m['one'] = 1
m['two'] = 2
println(m['one']) // "1"
println(m['bad_key']) // "0"
println('bad_key' in m) // Use `in` to detect whether such key exists
m.delete('two')

// Short syntax
numbers := {
    'one': 1,
    'two': 2
}
```

## If

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
Unlike other C-like languages, there are no parentheses surrounding the condition, and the braces are always required.

`if` can be used as an expression:

```v
num := 777
s := if num % 2 == 0 {
    'even'
}
else {
    'odd'
}
println(s) // "odd"
```

## In operator

`in` allows to check whether an array or a map contains an element.

```v
nums := [1, 2, 3]
println(1 in nums) // true

m := {'one': 1, 'two': 2}
println('one' in m) // true
```

It's also useful for writing clearer and more compact boolean expressions:

```v
if parser.token == .plus || parser.token == .minus ||
    parser.token == .div || parser.token == .mult {
    ...
}

if parser.token in [.plus, .minus, .div, .mult] {
    ...
}
```

V optimizes such expressions, so both `if` statements above produce the same machine code and no arrays are created.

## For loop

V has only one looping construct: `for`.

```v
numbers := [1, 2, 3, 4, 5]
for num in numbers {
    println(num)
}
names := ['Sam', 'Peter']
for i, name in names {
    println('$i) $name')  // Output: 0) Sam
}                             //         1) Peter
```

The `for value in` loop is used for going through elements of an array.
If an index is required, an alternative form `for index, value in` can be used.

Note, that the value is read-only. If you need to modify the array while looping, you have to use indexing:

```v
mut numbers := [1, 2, 3, 4, 5]
for i, num in numbers {
    println(num)
    numbers[i] = 0
}
```

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

```v
mut num := 0
for {
    num++
    if num >= 10 {
        break
    }
}
println(num) // "10"
```

The condition can be omitted, resulting in an infinite loop.

```v
for i := 0; i < 10; i++ {
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

## Match

```v
os := 'windows'
print('V is running on ')
match os {
    'darwin' { println('macOS.') }
    'linux'  { println('Linux.') }
    else     { println(os) }
}

number := 2
s := match number {
    1    { 'one' }
    2    { 'two' }
    else { 'many'}
}
```

A match statement is a shorter way to write a sequence of `if - else` statements.
When a matching branch is found, the following statement block will be run, and the final expression will be returned.
The else branch will be evaluated when no other branches match.

```v
enum Color {
    red
    blue
    green
}

fn is_red_or_blue(c Color) bool {
    return match c {
        .red  { true  }
        .blue { true  }
        else  { false }
    }
}
```

A match statement can also be used to branch on the variants of an `enum`
by using the shorthand `.variant_here` syntax.

## Structs

```v
struct Point {
    x int
    y int
}

p := Point{
    x: 10
    y: 20
}

println(p.x) // Struct fields are accessed using a dot
```

<p>&nbsp;</p>

Structs are allocated on the stack. To allocate a struct on the heap
and get a reference to it, use the `&` prefix:

```v
 // Alternative initialization syntax for structs with 3 fields or fewer
p := &Point{10, 10}
// References have the same syntax for accessing fields
println(p.x)
```

The type of `p` is `&Point`. It's a reference to `Point`.
References are similar to Go pointers and C++ references.

<p>&nbsp;</p>

V doesn't allow subclassing, but it supports embedded structs:

```v
// TODO: this will be implemented later
struct Button {
    Widget
    title string
}

button := new_button('Click me')
button.set_pos(x, y)

// Without embedding we'd have to do
button.widget.set_pos(x,y)
```

<p>&nbsp;</p>

```v
struct Foo {
    n   int      // n is 0 by default
    s   string   // s is '' by default
    a   []int    // a is `[]int{}` by default
    pos int = -1 // custom default value
}
```

All struct fields are zeroed by default during the creation of the struct. Array and map fields are allocated.

It's also possible to define custom default values.


## Short struct initialization syntax

There are no default function argument values or named arguments, for that the short struct initialization syntax can be used instead:

```v
struct ButtonConfig {
    text        string
    is_disabled bool
    width       int = 70
    height      int = 20
}

fn new_button(c ButtonConfig) &Button {
    return &Button{
        width: c.width
	height: c.height
	text: c.text
    }
}

button := new_button(text:'Click me', width:100) // the height is unset, so it's 20, the default value
```

As you can see, we can use

```
new_button(text:'Click me', width:100)
```

instead of

```
new_button(ButtonConfig{text:'Click me', width:100})
```

This only works with functions that have a single struct argument.

## Access modifiers

Struct fields are private and immutable by default (making structs immutable as well).
Their access modifiers can be changed with
`pub` and `mut`. In total, there are 5 possible options:

```v
struct Foo {
    a int   // private immutable (default)
mut:
    b int   // private mutable
    c int   // (you can list multiple fields with the same access modifier)
pub:
    d int   // public immmutable (readonly)
pub mut:
    e int   // public, but mutable only in parent module
__global:
    f int   // public and mutable both inside and outside parent module
}           // (not recommended to use, that's why the 'global' keyword
            // starts with __)
```

For example, here's the `string` type defined in the `builtin` module:

```v
struct string {
    str byteptr
pub:
    len int
}
```

It's easy to see from this definition that `string` is an immutable type.
The byte pointer with the string data is not accessible outside `builtin` at all.
The `len` field is public, but immutable:
```v
fn main() {
    str := 'hello'
    len := str.len // OK
    str.len++      // Compilation error
}
```

This means that defining public readonly fields is very easy in V, no need in getters/setters or properties.

## Methods

```v
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

V doesn't have classes. But you can define methods on types.

A method is a function with a special receiver argument.

The receiver appears in its own argument list between the `fn` keyword and the method name.

In this example, the `can_register` method has a receiver of type `User` named `u`.
The convention is not to use receiver names like `self` or `this`,
but a short, preferably one letter long, name.

## Pure functions by default

V functions are pure by default, meaning that their return values are a function of their arguments only,
and their evaluation has no side effects.

This is achieved by lack of global variables and all function arguments being immutable by default,
even when references are passed.

V is not a purely functional language however.

It is possible to modify function arguments by using the keyword `mut`:

```v
struct User {
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
    for i in 0..arr.len {
        arr[i] *= 2
    }
}

mut nums := [1, 2, 3]
multiply_by_2(mut nums)
println(nums) // "[2, 4, 6]"
```

Note, that you have to add `mut` before `nums` when calling this function. This makes
it clear that the function being called will modify the value.

It is preferable to return values instead of modifying arguments.
Modifying arguments should only be done in performance-critical parts of your application
to reduce allocations and copying.

For this reason V doesn't allow the modification of arguments with primative types such as integers. Only more complex types such as arrays and maps may be modified.

Use `user.register()` or `user = register(user)`
instead of `register(mut user)`.

V makes it easy to return a modified version of an object:

```v
fn register(u User) User {
    return { u | is_registered: true }
}

user = register(user)
```

## Anonymous & high order functions

```v
fn sqr(n int) int {
    return n * n
}

fn run(value int, op fn(int) int) int {
    return op(value)
}

fn main()  {
    println(run(5, sqr)) // "25"

    // Anonymous functions can be declared inside other functions:
    double_fn := fn(n int) int {
        return n + n
    }
    println(run(5, double_fn)) // "10"

    // Functions can be passed around without assigning them to variables:
    res := run(5, fn(n int) int {
        return n + n
    })
}
```

## References

```v
fn (foo Foo) bar_method() {
    ...
}

fn bar_function(foo Foo) {
    ...
}
```

If a function argument is immutable (like `foo` in the examples above)
V can pass it either value or reference. The compiler will determine this by itself,
and the developer doesn't need to think about it.

You no longer need to remember whether you should pass the struct by value
or by reference.

You can ensure that the struct is always passed by reference by
adding `&`:

```v
fn (foo &Foo) bar() {
    println(foo.abc)
}
```

`foo` is still immutable and can't be changed. For that,
`(mut foo Foo)` has to be used.

In general, V's references are similar to Go pointers and C++ references.
For example, a tree structure definition would look like this:

```v
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

pub fn (c Color) str() string { return '{$c.r, $c.g, $c.b}' }

fn rgb(r, g, b int) Color { return Color{r: r, g: g, b: b} }

const (
    numbers = [1, 2, 3]

    red  = Color{r: 255, g: 0, b: 0}
    blue = rgb(0, 0, 255)
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

```v
println('Top cities: $TOP_CITIES.filter(.usa)')
vs
println('Top cities: $top_cities.filter(.usa)')
```

## println

`println` is a simple yet powerful builtin function. It can print anything:
strings, numbers, arrays, maps, structs.

```v
println(1) // "1"
println('hi') // "hi"
println([1,2,3]) // "[1, 2, 3]"
println(User{name:'Bob', age:20}) // "User{name:'Bob', age:20}"
```

If you want to define a custom print value for your type, simply define a
`.str() string` method.

If you don't want to print a newline, use `print()` instead.

## Modules

V is a very modular language. Creating reusable modules is encouraged and is
very simple.
To create a new module, create a directory with your module's name and
.v files with code:

```v
cd ~/code/modules
mkdir mymodule
vim mymodule/mymodule.v

// mymodule.v
module mymodule

// To export a function we have to use `pub`
pub fn say_hi() {
    println('hello from mymodule!')
}
```

You can have as many .v files in `mymodule/` as you want.

That's it, you can now use it in your code:

```v
module main

import mymodule

fn main() {
    mymodule.say_hi()
}
```

Note that you have to specify the module every time you call an external function.
This may seem verbose at first, but it makes code much more readable
and easier to understand, since it's always clear which function from
which module is being called. Especially in large code bases.

Module names should be short, under 10 characters. Circular imports are not allowed.

You can create modules anywhere.

All modules are compiled statically into a single executable.

If you want to write a module that will automatically call some
setup/initialization code when imported (perhaps you want to call
some C library functions), write a module `init` function inside the module:

```v
fn init() int {
    // your setup code here ...
    return 1
}
```

The init function cannot be public. It will be called automatically.

## Interfaces

```v
struct Dog {}
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

fn perform(s Speaker) string {
    if s is Dog { // use `is` to check the underlying type of an interface
        println('perform(dog)')
    } else if s is Cat {
        println('perform(cat)')
    }
    return s.speak()
}

dog := Dog{}
cat := Cat{}
println(perform(dog)) // "woof"
println(perform(cat)) // "meow"
```

A type implements an interface by implementing its methods.
There is no explicit declaration of intent, no "implements" keyword.

## Enums

```v
enum Color {
    red green blue
}

mut color := Color.red
// V knows that `color` is a `Color`. No need to use `color = Color.green` here.
color = .green
println(color) // "1"  TODO: print "green"?
```

## Sum types

```v
type Expr = BinaryExpr | UnaryExpr | IfExpr

struct BinaryExpr{ ... }
struct UnaryExpr{ ... }
struct IfExpr{ ... }

struct CallExpr {
	args []Expr
	...
}

fn (p mut Parser) expr(precedence int) Expr {
	match p.tok {
		.key_if { return IfExpr{} }
		...
		else    { return BinaryExpr{} }
	}
}

fn gen(expr Expr) {
	match expr {
		IfExpr { gen_if(it) }
		...
	}
}

fn gen_if(expr IfExpr) {
	...
}
```

To check whether a sum type is a certain type, use `is`:

```v
println(expr is IfExpr)
```

## Option/Result types and error handling

```v
struct User {
    id int
    name string
}

struct Repo {
    users []User
}

fn new_repo() Repo {
    return Repo {
        users: [User{1, 'Andrew'}, User {2, 'Bob'}, User {10, 'Charles'}]
    }
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
    repo := new_repo()
    user := repo.find_user_by_id(10) or { // Option types must be handled by `or` blocks
        return  // `or` block must end with `return`, `break`, or `continue`
    }
    println(user.id) // "10"
    println(user.name) // "Charles"
}
```

V combines `Option` and `Result` into one type, so you don't need to decide which one to use.

The amount of work required to "upgrade" a function to an optional function is minimal;
you have to add a `?` to the return type and return an error when something goes wrong.

If you don't need to return an error message, you can simply `return none` (this is a more efficient equivalent of `return error("")`).

This is the primary mechanism for error handling in V. They are still values, like in Go,
but the advantage is that errors can't be unhandled, and handling them is a lot less verbose.

`err` is defined inside an `or` block and is set to the string message passed
to the `error()` function. `err` is empty if `none` was returned.

```v
user := repo.find_user_by_id(7) or {
    println(err) // "User 7 not found"
    return
}
```

You can also propagate errors:

```v
resp := http.get(url)?
println(resp.text)
```

`http.get` returns `?http.Response`. Because it was called with `?`, the error will be propagated to the calling function
(which must return an optional). If it is used in the `main()` function it will cause a panic.

The code above is essentially a condensed version of

```v
resp := http.get(url) or {
    return error(err)
}
println(resp.text)
```

V does not have a way to forcibly "unwrap" an optional (as other languages do, for instance Rust's `unwrap()`
or Swift's `!`). To do this use `or { panic(err) }` instead.

## Generics

```v
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
users_repo := new_repo<User>(db)
posts_repo := new_repo<Post>(db)
user := users_repo.find_by_id(1)?
post := posts_repo.find_by_id(1)?
```

## Concurrency

V's model of concurrency is very similar to Go's. To run `foo()` concurrently, just
call it with `go foo()`. Right now, it launches the function on a new system
thread. Soon coroutines and a scheduler will be implemented.

## Decoding JSON

```v
import json

struct User {
    name string
    age  int

    // Use the `skip` attribute to skip certain fields
    foo Foo [skip]

    // If the field name is different in JSON, it can be specified
    last_name string [json:lastName]
}

data := '{ "name": "Frodo", "lastName": "Baggins", "age": 25 }'
user := json.decode(User, data) or {
    eprintln('Failed to decode json')
    return
}
println(user.name)
println(user.last_name)
println(user.age)
```

Because of the ubiquitous nature of JSON, support for it is built directly into V.

The `json.decode` function takes two arguments: the first argument of the `json.decode` function is the type into which the JSON value should be decoded and the second is a string containing the JSON data.

V generates code for JSON encoding and decoding. No runtime reflection is used. This results in much better
performance.

## Testing

```v
// hello.v
fn hello() string {
    return 'Hello world'
}

// hello_test.v
fn test_hello() {
    assert hello() == 'Hello world'
}
```

The `assert` keyword can be used outside of tests as well.

All test functions have to be placed in files named `<some name>_test.v` and test function names must begin with `test_`.

You can also define a special test function: `testsuite_begin`, which will be
run *before* all other test functions in a `_test.v` file.

You can also define a special test function: `testsuite_end`, which will be
run *after* all other test functions in a `_test.v` file.

To run the tests do `v hello_test.v`.

To test an entire module, do `v test mymodule`.

You can also do `v test .` to test everything inside your curent folder (and subdirectories).

You can pass `-stats` to v test, to see more details about the individual tests in each _test.v file.

## Memory management

(Work in progress)

V doesn't use garbage collection or reference counting. The compiler cleans everything up
during compilation. If your V program compiles, it's guaranteed that it's going
to be leak free. For example:

```v
fn draw_text(s string, x, y int) {
    ...
}

fn draw_scene() {
    ...
    draw_text('hello $name1', 10, 10)
    draw_text('hello $name2', 100, 10)
    draw_text(strings.repeat('X', 10000), 10, 50)
    ...
}
```

The strings don't escape `draw_text`, so they are cleaned up when
the function exits.

In fact, the first two calls won't result in any allocations at all.
These two strings are small,
V will use a preallocated buffer for them.

```v
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

## Defer

A defer statement defers the execution of a block of statements until the surrounding function returns.

```v
fn read_log() {
    f := os.open('log.txt')
    defer { f.close() }
    ...
    if !ok {
        // defer statement will be called here, the file will be closed
        return
    }
    ...
    // defer statement will be called here, the file will be closed
}
```

## ORM

(this is still in an alpha state)

V has a built-in ORM (object-relational mapping) which supports Postgres, and will soon support MySQL and SQLite.

V's ORM provides a number of benefits:

- One syntax for all SQL dialects. Migrating between databases becomes much easier.
- Queries are constructed using V's syntax. There's no need to learn another syntax.
- Safety. All queries are automatically santised to prevent SQL injection.
- Compile time checks. This prevents typos which can only be caught during runtime.
- Readability and simplicity. You don't need to manually parse the results of a query and then manually construct objects from the parsed results.

```v
struct Customer { // struct name has to be the same as the table name (for now)
    id int // an field named `id` of integer type must be the first field
    name string
    nr_orders int
    country string
}

db := pg.connect(db_name, db_user)

// select count(*) from Customer
nr_customers := db.select count from Customer
println('number of all customers: $nr_customers')

// V syntax can be used to build queries
// db.select returns an array
uk_customers := db.select from Customer where country == 'uk' && nr_orders > 0
println(uk_customers.len)
for customer in uk_customers {
    println('$customer.id - $customer.name')
}

// by adding `limit 1` we tell V that there will be only one object
customer := db.select from Customer where id == 1 limit 1
println('$customer.id - $customer.name')

// insert a new customer
new_customer := Customer{name: 'Bob', nr_orders: 10}
db.insert(new_customer)
```

## vfmt

You don't need to worry about formatting your code or setting style guidelines.
`vfmt` takes care of that:

```v
v fmt file.v
```

It's recommended to set up your editor, so that vfmt runs on every save.
A vfmt run is usually pretty cheap (takes <30ms).

Always run `v fmt file.v` before pushing your code.

## Writing Documentation

The way it works is very similar to Go. It's very simple: there's no need to
write documentation seperately for your code, vdoc will generate it from docstrings in the source code.

Documentation for each function/type/const must be placed right before the declaration:

```v
// clearall clears all bits in the array
fn clearall() {

}
```

The comment must start with the name of the definition.

An overview of the module must be placed in the first comment right after the module's name.

To generate documentation use vdoc, for example `v doc net.http`.

## Profiling

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
fn main(){
    sw := time.new_stopwatch({})
    println('Hello world')
    println('Greeting the world took: ${sw.elapsed().nanoseconds()}ns')
}
```

# Advanced Topics

## Calling C functions from V

```v
#flag -lsqlite3
#include "sqlite3.h"

struct C.sqlite3
struct C.sqlite3_stmt

fn C.sqlite3_open(charptr, C.sqlite3)
fn C.sqlite3_column_int(stmt C.sqlite3_stmt, n int) int
// Or just define the type of parameter & leave C. prefix
fn C.sqlite3_prepare_v2(sqlite3, charptr, int, sqlite3_stmt, charptr) int
fn C.sqlite3_step(sqlite3)
fn C.sqlite3_finalize(sqlite3_stmt)

fn main() {
    path := 'users.db'
    db := &C.sqlite3(0) // a temporary hack meaning `sqlite3* db = 0`
    C.sqlite3_open(path.str, &db)
    query := 'select count(*) from users'
    stmt := &C.sqlite3_stmt(0)
    C.sqlite3_prepare_v2(db, query.str, - 1, &stmt, 0)
    C.sqlite3_step(stmt)
    nr_users := C.sqlite3_column_int(stmt, 0)
    C.sqlite3_finalize(stmt)
    println(nr_users)
}
```

Add `#flag` directives to the top of your V files to provide C compilation flags like:

- `-I` for adding C include files search paths
- `-l` for adding C library names that you want to get linked
- `-L` for adding C library files search paths
- `-D` for setting compile time variables

You can use different flags for different targets. Currently the `linux`, `darwin` , `freebsd`, and `windows` flags are supported.

NB: Each flag must go on its own line (for now)

```v
#flag linux -lsdl2
#flag linux -Ivig
#flag linux -DCIMGUI_DEFINE_ENUMS_AND_STRUCTS=1
#flag linux -DIMGUI_DISABLE_OBSOLETE_FUNCTIONS=1
#flag linux -DIMGUI_IMPL_API=
```

You can also include C code directly in your V module. For example, let's say that your C code is located in a folder named 'c' inside your module folder. Then:

* Put a v.mod file inside the toplevel folder of your module (if you
created your module with `v create` you already have v.mod file). For
example:
```v
Module {
	name: 'mymodule',
	description: 'My nice module wraps a simple C library.',
	version: '0.0.1'
	dependencies: []
}
```


* Add these lines to the top of your module:
```v
#flag -I @VROOT/c
#flag @VROOT/c/implementation.o
#include "header.h"
```
NB: @VROOT will be replaced by V with the *nearest parent folder, where there is a v.mod file*.
Any .v file beside or below the folder where the v.mod file is, can use #flag @VROOT/abc to refer to this folder.
The @VROOT folder is also *prepended* to the module lookup path, so you can *import* other
modules under your @VROOT, by just naming them.

The instructions above will make V look for an compiled .o file in your module folder/c/implementation.o .
If V finds it, the .o file will get linked to the main executable, that used the module.
If it does not find it, V assumes that there is a `@VROOT/c/implementation.c` file,
and tries to compile it to a .o file, then will use that.

This allows you to have C code, that is contained in a V module, so that its distribution is easier.
You can see a complete example for using C code in a V wrapper module here:
[minimal V project, that has a module, which contains C code](https://github.com/vlang/v/tree/master/vlib/v/tests/project_with_c_code)

You can use `-cflags` to pass custom flags to the backend C compiler. You can also use `-cc` to change the default C backend compiler.
For example: `-cc gcc-9 -cflags -fsanitize=thread`.

Ordinary zero terminated C strings can be converted to V strings with `string(cstring)` or `string(cstring, len)`.

NB: `string/1` and `string/2` do NOT create a copy of the `cstring`, so you should NOT free it after calling `string()`. If you need to make a copy of the C string (some libc APIs like `getenv/1` pretty much require that, since they
return pointers to internal libc memory), you can use: `cstring_to_vstring(cstring)`

On Windows, C APIs often return so called `wide` strings (utf16 encoding).
These can be converted to V strings with `string_from_wide(&u16(cwidestring))` .

V has these types for easier interoperability with C:

- `voidptr` for C's `void*`,
- `byteptr` for C's `byte*` and
- `charptr` for C's `char*`.
- `&charptr` for C's `char**`

To cast a `voidptr` to a V reference, use `user := &User(user_void_ptr)`.

`voidptr` can also be dereferenced into a V struct through casting: `user := User(user_void_ptr)`.

[Socket.v has an example which calls C code from V](https://github.com/vlang/v/blob/master/vlib/net/socket.v) .

To debug issues in the generated C code, you can pass these flags:

- `-cg` - produces a less optimized executable with more debug information in it.
- `-keepc` - keep the generated C file, so your debugger can also use it.
- `-showcc` - prints the C command that is used to build the program.

For the best debugging experience, you can pass all of them at the same time: `v -cg -keepc -showcc yourprogram.v` , then just run your debugger (gdb/lldb) or IDE on the produced executable `yourprogram`.

If you just want to inspect the generated C code, without further compilation, you can also use the `-o` flag (e.g. `-o file.c`). This will make V produce the `file.c` then stop.

If you want to see the generated C source code for *just* a single C function, for example `main`, you can use: `-printfn main -o file.c` .

To see a detailed list of all flags that V supports, use `v help`, `v help build`, `v help build-c` .

## Conditional compilation

```v
$if windows {
    println('Windows')
}
$if linux {
    println('Linux')
}
$if macos {
    println('macOS')
}

$if debug {
    println('debugging')
}
```

If you want an `if` to be evaluated at compile time it must be prefixed with a `$` sign. Right now it can only be used to detect
an OS or a `-debug` compilation option.

## Compile time pseudo variables

V also gives your code access to a set of pseudo string variables, that are substituted at compile time:

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
eprintln( 'file: ' + @FILE + ' | line: ' + @LINE + ' | fn: ' + @MOD + '.' + @FN)
```

Another example, is if you want to embed the version/name from v.mod *inside* your executable:
```v
import v.vmod
vm := vmod.decode( @VMOD_FILE ) or { panic(err) }
eprintln('$vm.name $vm.version\n $vm.description')
```

## Reflection via codegen

Having built-in JSON support is nice, but V also allows you to create efficient
serializers for any data format:

```v
// TODO: not implemented yet
fn decode<T>(data string) T {
    mut result := T{}
    for field in T.fields {
        if field.typ == 'string' {
            result.$field = get_string(data, field.name)
        } else if field.typ == 'int' {
            result.$field = get_int(data, field.name)
        }
    }
    return result
}

// generates to:
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
    return Vec {
        a.x + b.x,
        a.y + b.y
    }
}

fn (a Vec) - (b Vec) Vec {
    return Vec {
        a.x - b.x,
        a.y - b.y
    }
}

fn main() {
    a := Vec{2, 3}
    b := Vec{4, 5}
    println(a + b) // "{6, 8}"
    println(a - b) // "{-2, -2}"
}
```

Operator overloading goes against V's philosophy of simplicity and predictability. But since
scientific and graphical applications are among V's domains, operator overloading is an important feature to have
in order to improve readability:

`a.add(b).add(c.mul(d))` is a lot less readable than `a + b + c * d`.

To improve safety and maintainability, operator overloading is limited:

- It's only possible to overload `+, -, *, /` operators.
- Calling other functions inside operator functions is not allowed.
- Operator functions can't modify their arguments.
- Both arguments must have the same type (just like with all operators in V).

## Inline assembly

TODO: not implemented yet

```v
fn main() {
    a := 10
    asm x64 {
        mov eax, [a]
        add eax, 10
        mov [a], eax
    }
}
```

## Translating C/C++ to V

TODO: translating C to V will be available in V 0.3. C++ to V will be available later this year.

V can translate your C/C++ code to human readable V code.
Let's create a simple program `test.cpp` first:

```v
#include <vector>
#include <string>
#include <iostream>

int main() {
        std::vector<std::string> s;
        s.push_back("V is ");
        s.push_back("awesome");
        std::cout << s.size() << std::endl;
        return 0;
}
```

Run `v translate test.cpp` and V will generate `test.v`:

```v
fn main {
    mut s := []
    s << 'V is '
    s << 'awesome'
    println(s.len)
}
```

An online C/C++ to V translator is coming soon.

When should you translate C code and when should you simply call C code from V?

If you have well-written, well-tested C code, then of course you can always simply call this C code from V.

Translating it to V gives you several advantages:

- If you plan to develop that code base, you now have everything in one language, which is much safer and easier to develop in than C.
- Cross-compilation becomes a lot easier. You don't have to worry about it at all.
- No more build flags and include files either.

## Hot code reloading

```v
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

```v
v -os windows .
```

or

```v
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

```v
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

## Appendix I: Keywords

V has 23 keywords:

```v
break
const
continue
defer
else
enum
fn
for
go
goto
if
import
in
interface
match
module
none
or
pub
return
struct
type
var
```

## Appendix II: Operators

```v
+    sum                    integers, floats, strings
-    difference             integers, floats
*    product                integers, floats
/    quotient               integers, floats
%    remainder              integers

&    bitwise AND            integers
|    bitwise OR             integers
^    bitwise XOR            integers

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
