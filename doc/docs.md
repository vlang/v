# V Documentation

## Introduction

V is a statically typed compiled programming language designed for building maintainable software.

It's similar to Go and is also influenced by Oberon, Rust, Swift.

V is a very simple language. Going through this documentation will take you about half an hour,
and by the end of it you will learn pretty much the entire language.

Despite being simple, it gives a lot of power to the developer. Anything you can do in other languages,
you can do in V.

## Hello World

```v
fn main() {
    println('hello world')
}
```

Functions are declared with `fn`. Return type goes after the function
name. In this case `main` doesn't return anything, so the type is
omitted.

Just like in C and all related languages, `main` is an entry point.

`println` is one of the few built-in functions. It prints the value
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

```v
fn foo() (int, int) {
    return 2, 3
}

a, b := foo()
println(a) // 2
println(b) // 3
```

Functions can return multiple values.
Functions, like consts, and types, are private (not exported) by default.
To allow other modules to use them, prepend `pub`. The same applies
to consts and types.

```v
pub fn public_function() {
}

fn private_function() {
}
```

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

```v
mut age := 20
println(age)
age = 21
println(age)
```

To change the value of the variable use `=`. In V, variables are
immutable by default. To be able to change the value of the variable, you have to declare it with `mut`.

Try compiling the program above after removing `mut` from the first line.

Please note the difference between `:=` and `=`  
`:=` is used for declaring and initializing, `=` is used for assigning.

```v
fn main() {
    age = 21
}
```

This code will not compile, because variable `age` is not declared.
All variables need to be declared in V.

```v
fn main() {
    age := 21
}
```

In development mode this code will result in an "unused variable" warning.
In production mode (`v -prod foo.v`) it will not compile at all, like in Go.

```v
fn main() {
    a := 10
    if true {
        a := 20
    }
}
```

Unlike most languages, variable shadowing is not allowed. Declaring a variable with a name that is already used in a parent scope will result in a compilation error.

## Basic types

```v
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

All operators in V must have values of the same type on both sides. This code will not compile if `age` is an `int`:

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

// We can also preallocate a certain amount of elements.
ids := [0].repeat(50) // This creates an array with 50 zeros
```

Array type is determined by the first element: `[1, 2, 3]` is an array of ints (`[]int`).

`['a', 'b']` is an array of strings (`[]string`).

All elements must have the same type. `[1, 'a']` will not compile.

`<<` is an operator that appends a value to the end of the array.
It can also append an entire array.

`.len` field returns the length of the array. Note, that it's a read-only field,
and it can't be modified by the user. All exported fields are read-only by default in V.

`val in array` returns true if the array contains `val`.

All arrays can be easily printed with `println(arr)` and converted to a string
with `s := arr.str()`.

Arrays can be efficiently filtered and mapped with `.filter()` and
`.map()` methods:

```v
nums := [1, 2, 3, 4, 5, 6]
even := nums.filter(it % 2 == 0)
println(even) // [2, 4, 6]

words := ['hello', 'world']
upper := words.map(it.to_upper())
println(upper) // ['HELLO', 'WORLD']
```

`it` is a special variable that refers to an element in filter/map methods.

## Maps

```v
mut m := map[string]int // Only maps with string keys are allowed for now
m['one'] = 1
m['two'] = 2
println(m['one']) // "1"
println(m['bad_key']) // "0"
println('bad_key' in m) // Use `in` to detect whether such key exists
m.delete('two')

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

It's also useful for writing more clear and compact boolean expressions:

```v
if parser.token == .plus || parser.token == .minus ||
    parser.token == .div || parser.token == .mult {
    ...
}

if parser.token in [.plus, .minus, .div, .mult] {
    ...
}
```

V optimizes such expressions, so both `if` statements above produce the same machine code, no arrays are created.

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

The condition can be omitted, this results in an infinite loop.

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

V doesn't have subclassing, but it supports embedded structs:

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
`len` field is public, but not mutable:

```v
fn main() {
    str := 'hello'
    len := str.len // OK
    str.len++      // Compilation error
}
```

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

V functions are pure by default, meaning that their return values are only determined by their arguments,
and their evaluation has no side effects.

This is achieved by lack of global variables and all function arguments being immutable by default,
even when references are passed.

V is not a pure functional language however.
It is possible to modify function arguments by using the same keyword `mut`:

```v
struct User {
mut:
    is_registered bool
}

fn (u mut User) register() {
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
fn multiply_by_2(arr mut []int) {
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

For this reason V doesn't allow to modify primitive args like integers, only
complex types like arrays and maps.

Use `user.register()` or `user = register(user)`
instead of `register(mut user)`.

V makes it easy to return a modified version of an object:

```v
fn register(u User) User {
    return { u | is_registered: true }
}

user = register(user)
```

## High order functions

```v
fn sqr(n int) int {
    return n * n
}

fn run(value int, op fn(int) int) int {
    return op(value)
}

fn main()  {
    println(run(5, sqr)) // "25"
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

If a function argument is immutable like `foo` in the examples above,
V can pass it by value or by reference. The decision is made
by the compiler, and the developer doesn't need to think about it.

You no longer need to remember whether you should pass the struct by value
or by reference.

There's a way to ensure that the struct is always passed by reference by
adding `&`:

```v
fn (foo &Foo) bar() {
    println(foo.abc)
}
```

`foo` is still immutable and can't be changed. For that,
`(foo mut Foo)` has to be used.

In general, V references are similar to Go pointers and C++ references.
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

When naming constants, snake_case must be used.
Many people prefer all caps consts: `TOP_CITIES`. This wouldn't work
well in V, because consts are a lot more powerful than in other languages.
They can represent complex structures, and this is used quite often since there
are no globals:

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

## Option/Result types & error handling

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

The amount of work required to "upgrade" a function to an optional function is minimal:
you have to add a `?` to the return type and return an error when something goes wrong.

If you don't need to return an error, you can simply `return none`.

This is the primary way of handling errors in V. They are still values, like in Go,
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
println(resp.body)
```

`http.get` returns `?http.Response`. It was called with `?`, so the error is propagated to the calling function
(which must return an optional) or in case of `main` leads to a panic.
Basically the code above is a shorter version of

```v
resp := http.get(url) or {
    panic(err)
}
println(resp.body)
```

V does not have a way to force unwrap an optional (like Rust's `unwrap()`
or Swift's `!`). You have to use `or { panic(err) }` instead.

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

The concurrency model is very similar to Go. To run `foo()` concurrently, just
call it with `go foo()`. Right now, it launches the function in a new system
thread. Soon coroutines and the scheduler will be implemented.

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

JSON is very popular nowadays, that's why JSON support is built in.

The first argument of the `json.decode` function is the type to decode to.
The second argument is the JSON string.

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

`assert` keyword can be used outside of tests as well.

All test functions have to be placed in `*_test.v` files and begin with `test_`.

You can also define a special test function: `testsuite_begin`, which will be 
run *before* all other test functions in a `_test.v` file.

You can also define a special test function: `testsuite_end`, which will be 
run *after* all other test functions in a `_test.v` file.

To run the tests do `v hello_test.v`.

To test an entire module, do `v test mymodule`.

You can also do `v test .` to test everything inside your curent folder (and underneath it).

You can pass `-stats` to v test, to see more details about the individual tests in each _test.v file.

## Memory management

(Work in progress)
There's no garbage collection or reference counting. V cleans everything up
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

(alpha)

V has a built-in ORM that supports Postgres, and will soon support MySQL and SQLite.

The benefits of V ORM:

- One syntax for all SQL dialects. Migrating to a different database becomes much easier.
- Queries are constructed with V syntax. There's no need to learn another syntax.
- Safety. It's impossible to construct a SQL query with an injection.
- Compile time checks. No more typos that can only be caught at runtime.
- Readability and simplicity. You don't need to manually parse the results and construct objects.

```v
struct Customer { // struct name has to be the same as the table name for now
    id int // an integer id must be the first field
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

You don't need to worry about formatting your code or style guidelines.
vfmt takes care of that:

```v
v fmt file.v
```

It's recommended to set up your editor, so that vfmt runs on every save.

Always run vfmt before pushing your code.

## writing_documentation

The way it works is very similar to Go. It's very simple: there's no need to
write documentation for your code, vdoc will generate it from the source code.

Documentation for each function/type/const must be placed right before the declaration:

```v
// clearall clears all bits in the array
fn clearall() {

}
```

The comment must start with the name of the definition.

An overview of the module must be placed in the first comment right after the module's name.

To generate documentation, run `v doc path/to/module` (TODO this is
temporarily disabled).

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

You can use different flags for different targets. Right now, `linux`, `darwin` , `freebsd`, and `windows` are supported.

NB: For now you have to use one flag per line:

```v
#flag linux -lsdl2
#flag linux -Ivig
#flag linux -DCIMGUI_DEFINE_ENUMS_AND_STRUCTS=1
#flag linux -DIMGUI_DISABLE_OBSOLETE_FUNCTIONS=1
#flag linux -DIMGUI_IMPL_API=
```

You can also add C code, in your V module. For example, lets say that your C code is located in a folder named 'c' inside your module folder. Then:

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
[minimal V project, that has a module, which contains C code](https://github.com/vlang/v/tree/master/vlib/compiler/tests/project_with_c_code)

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

To cast `voidptr` to V references, use `user := &User(user_void_ptr)`.

`voidptr` can also be dereferenced to V structs by casting: `user := User(user_void_ptr)`.

Check out [socket.v for an example of calling C code from V](https://github.com/vlang/v/blob/master/vlib/net/socket.v) .

To debug issues with the generated C code, you can pass these flags:

- `-cg` - produces a less optimized executable with more debug information in it.
- `-keep_c` - keep the generated C file, so your debugger can also use it.
- `-pretty_c` - run clang-format over the generated C file, so it looks nicer and is easier to read.
- `-show_c_cmd` - prints the C command that is used to build the program.

For best debugging experience, you can pass all of them at the same time: `v -cg -keep_c -pretty_c -show_c_cmd yourprogram.v` , then just run your debugger (gdb/lldb) or IDE with the produced executable `yourprogram`.

If you just want to inspect the generated C code, without compiling it further, you can also use: `-o file.c`. This will make V produce the `file.c` then stop.

## Compile time if

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

Compile time `if` starts with a `$`. Right now it can only be used to detect
an OS or a `-debug` compilation option.

## Reflection via codegen

Having built-in JSON support is nice, but V also allows you to create efficient
serializers for anything:

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
scientific and graphical applications are among V's domains, operator overloading is very important to have
in order to improve readability:

`a.add(b).add(c.mul(d))` is a lot less readable than `a + b + c * d`.

To improve safety and maintainability, operator overloading has several limitations:

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

Use .vsh file extension. It will make all functions in the `os`
module global (so that you can use `ls()` instead of `os.ls()`, for example).

```v
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
`v deploy.v && ./deploy`

Or just run it more like a traditional bash script:
`v run deploy.v`

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
mut
none
or
pub
return
struct
type
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
