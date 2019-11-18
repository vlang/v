# V Documentation

## Introduction

V is a statically typed compiled programming language designed for building maintainable software.
It's similar to Go and is also influenced by Oberon, Rust, Swift.

V is a very simple language. Going through this documentation will take you about half an hour, and by the end of it, you will learn pretty much the entire language.

Despite being simple, it gives a lot of power to the developer. Anything you can do in other languages, you can do in V.

## Hello World

```go
fn main() {
    println('hello world')
}
```

Functions are declared with `fn`. The return type goes after the function name. In this case, the `main` doesn't return anything, so the type is omitted.

Just like in C and all related languages, `main` is an entry point.

`println` is one of the few built-in functions. It prints the value to standard output.

`fn main()` declaration can be skipped in one file program. This is useful when writing small programs, "scripts", or just learning the language. For brevity, `fn main()` will be skipped in this tutorial.

This means that a "hello world" program can be as simple as

```go
println('hello world')
```

## Comments

```go
// This is a single line comment.

/* This is a multiline comment.
   /* It can be nested. */
*/
```

## Functions

```go
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

Just like in Go and C, functions cannot be overloaded. This simplifies the code and improves maintainability and readability.

Functions can be used before their declaration: `add` and `sub` are declared after `main`, but can still be called from `main`. This is true for all declarations in V and eliminates the need for header files or thinking about the order of files and declarations.

```go
fn foo() (int, int) {
    return 2, 3
}

a, b := foo()
println(a) // 2
println(b) // 3
```

Functions, like consts, and types, are private (not exported) by default. To allow other modules to use them, prepend `pub`. The same applies to consts and types.

```go
pub fn public_function() {

}

fn private_function() {

}
```

## Variables

```go
name := 'Bob'
age := 20
large_number := i64(9999999999)
println(name)
println(age)
println(large_number)
```

Variables are declared and initialized with `:=`. This is the only way to declare variables in V. This means that variables always have an initial value.

The variable's type is inferred from the value on the right hand side. To force a different type, use type conversion: the expression `T(v)` converts the value `v` to the type `T`.

Unlike most other languages, V only allows defining variables in functions. Global (module level) variables are not allowed. There's no global state in V.

```go
mut age := 20
println(age)
age = 21
println(age)
```

To change the value of the variable use `=`. In V, variables are immutable by default. To be able to change the value of the variable, you have to declare it with `mut`.

Try compiling the program above after removing `mut` from the first line.

Please note the difference between `:=` and `=`
`:=` is used for declaring and initializing, `=` is used for assigning.

```go
fn main() {
    age = 21
}
```

This code will not compile, because variable `age` is not declared. All variables need to be declared in V.

```go
fn main() {
    age := 21
}
```

In development mode, this code will result in an "unused variable" warning. In production mode (`v -prod foo.v`) it will not compile at all, like in Go.

```go
fn main() {
    a := 10
    if true {
        a := 20
    }
}
```

Unlike most languages, variable shadowing is not allowed. Declaring a variable with a name that is already used in a parent scope will result in a compilation error.

```go
bool

string

i8    i16  int  i64      i128 (soon)
byte  u16  u32  u64      u128 (soon)

rune // represents a Unicode code point

f32 f64

byteptr
voidptr
```

Please note that unlike C and Go, `int` is always a 32 bit integer.

## Strings

```go
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

Both single and double quotes can be used to denote strings. For consistency, `vfmt` converts double quotes to single quotes unless the string contains a single quote character.

Interpolation syntax is pretty simple. It also works with fields: `'age = $user.age'`. If you need more complex expressions, use `${}`: `'can register = \${user.age > 13}'`.

All operators in V must have values of the same type on both sides. This code will not compile if `age` is an `int`:

```go
println('age = ' + age)
We have to either convert age to a string:
println('age = ' + age.str())
or use string interpolation (preferred):
println('age = $age')
```

To denote character literals, use `

```go
a := `a`
assert 'aloha!'[0] == `a`
```
