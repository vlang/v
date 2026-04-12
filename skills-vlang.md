# skills.md — V Language Project Guide

Guidelines for AI coding agents working in this V (https://vlang.io) project.
Official docs: https://github.com/vlang/v/blob/master/doc/docs.md

## Build & Run Commands

```bash
# Initial build (only if `./v` is missing)
make                    # Linux/macOS
make.bat                # Windows

# Build a working compiler (recommended)
./v -g -keepc -o ./vnew cmd/v

# Compile and run a file
./vnew run file.v

# Compile only (produces executable)
./vnew file.v

# Run a project folder with multiple .v files
./vnew run .

# Cross-compile
./vnew -os windows .
./vnew -os linux .
```

**Rule:** Use `./v` only to build `./vnew`; use `./vnew` for everything else.
Never run `./v self` without `-o` — it overwrites the working compiler.

## Testing

```bash
# Run a single test file (shows test output)
./vnew path/to/file_test.v

# Run tests in a directory
./vnew -silent test path/to/dir/

# Run tests with timing stats
./vnew -stats test path/to/dir/

# Run compiler error tests
./vnew -silent vlib/v/compiler_errors_test.v

# Auto-fix .out files (run twice when intended behavior changes)
VAUTOFIX=1 ./vnew -silent vlib/v/compiler_errors_test.v

# Run ALL tests (ask before running — very slow)
./vnew test-all
```

### Test File Conventions
- Test files end with `_test.v` and contain `test_<name>()` functions
- Output-matching tests use `.vv` source + `.out` expected output pairs
- Use `assert` for assertions inside `test_` functions

### Useful Test Env Variables
| Variable | Effect |
|---|---|
| `VAUTOFIX=1` | Auto-update `.out` files on failure |
| `VTEST_ONLY=pattern` | Run only tests matching pattern |
| `VTEST_HIDE_OK=1` | Hide passing tests, show only failures |

## Code Style & Formatting

```bash
# Format a file (required before commit)
./vnew fmt -w file.v

# Verify all files are formatted
./vnew -silent test-fmt

# Check markdown files (lines <= 100 chars)
./vnew check-md file.md
```

### Naming Conventions
- **snake_case** for consts, variables, functions, fields: `get_string`, `nr_orders`
- **PascalCase** for types/structs: `User`, `Customer`, `EmbedFileData`
- Test functions prefixed with `test_`: `fn test_get_string() { ... }`
- Module names match their directory name (no hierarchy in `module` line)
- **No variable shadowing** — declaring a variable with a name already used in a
  parent scope is a compile error

### Imports
```v
import os
import net.http
import json

# Selective imports
import os { input, user_os }

# Module aliasing
import crypto.sha256 as mysha256
```
Imports follow folder hierarchy: `import abc.def` maps to `abc/def/`.
Symbols are called with single prefix: `def.func()`, not `abc.def.func()`.

### Variables & Immutability
#### Primitive types:
```v
bool
string 
i8 i16 i32 i64 
u8 u16 u32 u64 
rune
f32 f64
voidptr charptr
```

- Variables are declared with `:=` (always initialized)，No global varible!
- Variables are **immutable by default**; use `mut` for mutable:
  ```v
  name := 'V'          // immutable
  mut count := 0       // mutable
  count += 1
  ```
- `:=` is declaration+initialization; `=` is assignment only
- No global variables by default (can be enabled with flags for low-level apps)

### Strings
- Strings are **immutable** (read-only), UTF-8 encoded
- Use single quotes: `'hello'` (double quotes converted by vfmt)
- Raw strings: `r'hello\nworld'` (escapes not processed)
- String interpolation: `'Hello, ${name}!'` or `'age = ${user.age}'`
- Format specifiers: `'${x:.2}'`, `'${int(x):10}'`, `'${int(x):b}'`
- Concatenation: `s1 + s2`, append: `s += 'world'`
- Indexing gives bytes (`u8`), not runes: `s[0]` → `u8`
- Convert to runes: `s.runes()` → `[]rune`
- Multi-line string:
```
multi_lines := '
hello
world
.
```

### Runes
- Represent Unicode code points, alias for `u32`
- Denoted with backticks: `rocket := `🚀``
- Convert to string: `rune.str()`, to bytes: `rune.bytes()`

### Arrays
```v
mut nums := [10, 20, 30]
nums << 4                    // append element
nums << [5, 6, 7]            // append array
println(nums.len)            // length
println(nums.cap)            // capacity
println(20 in nums)          // true

# Array initialization with size
mut a := []int{len: 100, cap: 300, init: 3}

# Slices
println(nums[1..4])          // [20, 30, 4]
println(nums[..4])           // [10, 20, 30, 4]
println(nums[1..])           // [20, 30, 4, 5, 6, 7]

# Array methods
even := nums.filter(it % 2 == 0)
upper := words.map(it.to_upper())
nums.any(it == 2)            // true if any match
nums.all(it >= 2)            // true if all match
nums.sort()                  // sort ascending
nums.sort(a > b)             // sort descending
```

### Maps
```v
mut m := map[string]int{}
m['one'] = 1
val := m['bad_key'] or { panic('key not found') }
if v := m['abc'] { println(v) }    # check if key exists
println('key' in m)                # key exists?
println(m.keys())                  # get all keys
m.delete('one')

# Map literal
numbers := {
    'one': 1
    'two': 2
}
```

### Structs
```v
struct User {
    name string
    age  int
mut:
    is_registered bool
}

# Short struct literal syntax (field names can be omitted for matching variables)
user := User{
    name: 'Bob'
    age: 25
}

# Trailing struct literal (common in GUI code)
button := Button{text: 'Click'}

# Struct update syntax
user2 := User{
    ...user
    age: 30
}
```

### Functions & Methods
```v
# Function declaration
fn greet(name string) string {
    return 'Hello, ${name}!'
}

# Return multiple values
fn foo() (int, int) {
    return 2, 3
}
a, b := foo()
c, _ := foo()    # ignore second value

# Method (receiver by reference)
fn (u User) can_register() bool {
    return u.age >= 16
}

# Mutable method
fn (mut u User) register() {
    u.is_registered = true
}

# Anonymous functions
fn (x int) bool { return x > 0 }

# Closures and higher-order functions supported
    mut my_int := 1
    my_closure := fn [mut my_int] () {
      my_int++
      println(my_int) // prints 2
    }
    my_closure() 
    println(my_int) // prints 1
```

- Functions cannot be overloaded (simplifies code)
- Hoisting: functions can be called before declaration
- Function args are **immutable by default**; use `mut` for mutable args
- `pub fn func(...)...{...}` in module needs comment like `func returns ...`

### Attribute
V has several attributes that modify the behavior of functions and structs.

An attribute is a compiler instruction specified inside `@[]` right before a function/struct/enum 
declaration and applies only to the following declaration. Attributes with arguments support both 
name: value and call-style name(value) syntax. Call-style attributes can also use named arguments.

```
// The following struct must be allocated on the heap. Therefore, it can only be used as a
// reference (`&Window`) or inside another reference (`&OuterStruct{ Window{...} }`).
// See section "Stack and Heap"
@[heap]
struct Window {
  id i32 @[json: 'ID']
}

// Equivalent call-style syntax:
@[deprecated(msg: 'use new_function2() instead', after: '2021-05-27')]
fn legacy_function2_call_style() {}
```

### Compile time reflection
`$` is used as a prefix for compile time (also referred to as `comptime`) operations, such as 
the fields, attributes, values and methods of struct.

### Control Flow

#### If
```v
if a < b {
    println('${a} < ${b}')
} else if a > b {
    println('${a} > ${b}')
} else {
    println('${a} == ${b}')
}

# If expression (like ternary)
num := if a < b { a } else { b }

# If unwrap (for option/result types)
if resp := http.get(url) {
    println(resp.body)
}
```

#### Match (like switch)
```v
os_name := match os {
    .windows { 'Windows' }
    .linux { 'Linux' }
    .macos { 'macOS' }
    else { 'Unknown' }
}
```

#### For Loop
```v
# Traditional for
for i := 0; i < 10; i++ {
    println(i)
}

# While-style for
for a < b {
    a++
}

# Range for
for i in 0 .. 10 {
    println(i)
}

# For-each with index
for i, val in arr {
    println('${i}: ${val}')
}

# Infinite loop
for {
    # break to exit
}
```

#### Defer
```v
fn read_file() ?string {
    f := os.open('file.txt') or { return err }
    defer { f.close() }    # executed when function returns
    return f.read_all()
}
```

### Error Handling

V uses `?Type` (optional) and `!Type` (result) types. Errors **must** be handled:

```v
# Option/Result with `or {}` block
text := os.read_file('app.log') or {
    eprintln('failed: ${err}')
    return
}

# Propagate errors with `?`
fn load() ?string {
    return os.read_file('data.txt')
}

# Force unwrap (only when certain)
data := http.get('https://example.com')!

# Custom error types
struct MyError {}
fn (e MyError) str() string { return 'my error message' }

fn do_something() !int {
    return MyError{}
}
```

### Enums
```v
enum Color {
    red
    green
    blue
}

c := Color.red
match c {
    .red { println('it is red') }
    .green { println('it is green') }
    .blue { println('it is blue') }
}
```

### Interfaces
```v
interface Speaker {
    speak() string
}

fn (d Dog) speak() string { return 'woof' }
fn (c Cat) speak() string { return 'meow' }

# Any type implementing the interface can be used
fn make_speak(s Speaker) {
    println(s.speak())
}
```

### Sum Types
```v
type MySumType = int | string | MyStruct

fn handle(v MySumType) {
    match v {
        int { println('got int: ${v}') }
        string { println('got string: ${v}') }
        MyStruct { println('got struct: ${v}') }
    }
}
```

### Generics
```v
fn map[T, U](arr []T, f fn (T) U) []U {
    mut result := []U{len: arr.len}
    for i, val in arr {
        result[i] = f(val)
    }
    return result
}

struct Repo[T] {
    items []T
}
```

### Concurrency
```v
# Spawning OS concurrent tasks
spawn fetch_url('https://example.com')

# Channels
ch := chan int{cap: 10}
ch <- 42          # send
val := <-ch       # receive

# Shared objects
shared arr := []int{}
lock arr {
    arr << 1
}
```

## Safety Rules
- **No null** (allowed only in `unsafe` code)
- **No undefined values**
- **No variable shadowing**
- **No globals** (unless enabled via flag for low-level apps)
- **Bounds checking** enabled by default
- **Immutable by default** for variables, structs, and function arguments
- Minimize `unsafe {}` blocks; add comments explaining necessity

## Memory Management

Four modes available:
| Flag | Description |
|---|---|
| *(default)* | Tracing GC (default) |
| `-autofree` | Compiler inserts free calls (~90-100% coverage) |
| `-gc none` | Manual memory management |
| `-prealloc` | Arena allocation |

## Compile-Time Code (Comptime)

V uses `$` prefix for compile-time constructs. These are **not** runtime:

```v
# Compile-time conditionals
$if windows {
    # Windows-specific code
} $else $if linux {
    # Linux-specific code
}

# Custom flags: use `?` suffix
$if custom_flag ? {
    # Enabled with: v -d custom_flag
}

# Compile-time iteration over struct fields
$for field in MyStruct.fields {
    $if field.typ is string {
        println(field.name)
    }
}

# Compile-time functions
$embed_file('path')   # Embed file into binary
$env('VAR')           # Read env var at compile time
$d('ident', default)  # Read -d define with fallback
pub fn (app App) post_form(mut ctx Context) veb.Result {
  ...
  return $veb.html()  //parse and render `templates/post/form.html`
}
```

**Common mistake:** Using runtime `for` to iterate struct fields — use `$for` instead.

## Debugging the Compiler

```bash
# Debug with C line numbers
./vnew -keepc -cg run file.v

# Show C compiler command
./vnew -showcc file.v

# Trace compiler stages
./vnew -d trace_parser -d trace_checker -o ./w cmd/v
./w file.v

# Print specific C function
./vnew -printfn main__main -o file.c file.v
```

## V Project Structure
repo: https://github.com/vlang/v
| Directory | Purpose |
|---|---|
| `vlib/` | Standard library and compiler modules |
| `vlib/v/` | Compiler (scanner, parser, checker, gen) |
| `cmd/v/` | Compiler entry point |
| `cmd/tools/` | vfmt, vdoc, vup, etc. |
| `examples/` | Example programs |
| `thirdparty/` | Bundled C libraries (tcc, mbedtls, etc.) |

## Agent Safety Rules

1. **Do not overwrite `./v` binary.** Build to `./vnew` instead.
2. **Ask before large refactors** (>5 files or cross-directory changes).
3. **Only modify files required** for the task; avoid unrelated changes.
4. **Do not touch `thirdparty/`** unless explicitly requested.
5. **Format all touched `.v`/`.vsh` files** with `./vnew fmt -w`.
6. **Check markdown** with `./vnew check-md` for any `.md` changes.
7. **If compiler breaks**, recover with: `git stash && make && git stash apply`.

## Common V Flags

| Flag | Purpose |
|---|---|
| `-g` | Debug info (V line numbers) |
| `-cg` | Debug info (C line numbers) |
| `-keepc` | Keep generated C file |
| `-prod` | Optimized production build |
| `-o file` | Output path |
| `-cc clang` | Pick C compiler |
| `-cstrict` | Stricter C generation |
| `-skip-unused` | Skip unused modules |
| `-b js\|native\|wasm` | Select backend |
| `-d flag` | Define compile-time flag |
| `-os <os>` | Cross-compile target |

## ORM & Database

V has a built-in ORM that works with SQLite, PostgreSQL, MySQL, and other drivers.

### ORM Struct Definition

```v
import time

@[table: 'users']
struct User {
    id          int         @[primary; sql: serial]
    name        string
    age         int
    created_at  time.Time   @[default: 'CURRENT_TIME']
    updated_at  ?string     @[sql_type: 'TIMESTAMP']
    deleted_at  ?time.Time
    posts       []Post      @[fkey: 'author_id']
}

struct Post {
    id        int    @[primary; sql: serial]
    author_id int
    title     string
}
```

### ORM Field Attributes

| Attribute | Effect |
|---|---|
| `@[primary]` | Sets field as primary key |
| `@[sql: serial]` | Auto-increment (DB chooses column type) |
| `@[unique]` | Adds `UNIQUE` constraint |
| `@[unique: 'group_name']` | Adds field to a `UNIQUE` group |
| `@[skip]` or `@[sql: '-']` | Skips the field |
| `@[sql: 'col_name']` | Custom column name |
| `@[sql_type: 'SQL TYPE']` | Explicit SQL type |
| `@[default: 'raw_sql']` | SQL default clause (e.g. `'CURRENT_TIME'`) |
| `@[fkey: 'parent_id']` | Foreign key for array fields |
| `@[index]` | Creates index |
| `@[comment: 'text']` | Adds comment |

### Nullable Columns

Use option types (`?Type`) for nullable columns:
```v
struct User {
    name     string   // NOT NULL
    nickname ?string  // nullable
}
```

### ORM Usage

```v
import db.sqlite

db := sqlite.connect(':memory:')!
defer { db.close() }

// Create table
sql db {
    create table User
}!

// Insert
user := User{name: 'Alice', age: 30}
sql db {
    insert user into User
}!

// Select
users := sql db {
    select from User where age > 18 && name != 'Bob' limit 10
}!

// Select with ordering
users := sql db {
    select from User where age > 18 order by id desc
}!

// Aggregate functions
total := sql db {
    select count from User
}!
avg_age := sql db {
    select avg(age) from User
}!

// Update
sql db {
    update User set age = 31 where name == 'Alice'
}!

// Delete
sql db {
    delete from User where id > 100
}!
```

### Transactions

```v
import orm

// Helper-based
orm.transaction[int](mut db, fn (mut tx orm.Tx) !int {
    user := User{name: 'Bob'}
    sql tx {
        insert user into User
    }!
    return tx.last_id()
})!

// Manual control
mut tx := orm.begin(mut db)!
sql tx {
    update User set name = 'Charlie' where id == 1
}!
tx.commit()!
// or: tx.rollback()!
```

### SQLite Performance Tips

```v
import db.sqlite

db := sqlite.connect('app.db')!
db.synchronization_mode(sqlite.SyncMode.off)!
db.journal_mode(sqlite.JournalMode.memory)!
```

## Web Framework (veb)

`Veb` is a buildin, fast, precompiled-template web framework with routing, middleware, and controllers.

### Basic web App Structure

```v
module main

import veb

pub struct Context {
    veb.Context
pub mut:
    user User
}

pub struct App {
    secret_key string
}

pub fn (app &App) index(mut ctx Context) veb.Result {
    return ctx.html('<h1>Hello V!</h1>')
}

fn main() {
    mut app := &App{secret_key: 'secret'}
    veb.run[App, Context](mut app, 8080)
}
```

### Routes & HTTP Verbs

```v
// Auto-mapped: GET /hello
pub fn (app &App) hello(mut ctx Context) veb.Result {
    return ctx.text('Hello')
}

// Custom path: GET /foo
@['/foo']
pub fn (app &App) world(mut ctx Context) veb.Result {
    return ctx.text('World')
}

// HTTP verb restriction
@[get]
pub fn (app &App) get_only(mut ctx Context) veb.Result {
    return ctx.text('GET only')
}

@['/login'; get; post]
pub fn (app &App) login(mut ctx Context) veb.Result {
    if ctx.req.method == .get {
        return ctx.html('<h1>Login</h1>')
    }
    // POST handling
    password := ctx.form['password']
    return ctx.redirect('/profile', typ: .see_other)
}

@['page/time']
pub fn (app App) page_time(mut ctx Context) veb.Result {
  // 
  return $veb.html() // parse and render src/templates/page/time.html
}
```

### Route Parameters

```v
@['/hello/:user']
pub fn (app &App) hello_user(mut ctx Context, user string) veb.Result {
    return ctx.text('Hello ${user}')
}

@['/doc/:id']
pub fn (app &App) get_doc(mut ctx Context, id int) veb.Result {
    return ctx.text('Doc ${id}')
}

// Wildcard route
@['/:path...']
pub fn (app &App) wildcard(mut ctx Context, path string) veb.Result {
    return ctx.text('Path: ${path}')
}
```

### Query, Form & Files

```v
@['/user'; get]
pub fn (app &App) get_user(mut ctx Context) veb.Result {
    name := ctx.query['name'] or {
        return ctx.text('no name')
    }
    return ctx.text('Hello ${name}')
}

// Access form data
username := ctx.form['username']
// Access uploaded files
files := ctx.files
```

### Response Methods

```v
ctx.html('<h1>HTML</h1>')          // text/html
ctx.text('plain text')             // text/plain
ctx.json(User{name: 'Bob'})        // application/json
ctx.no_content()                   // 204 No Content
ctx.file('data/image.png')         // serve file
ctx.redirect('/path', typ: .found) // redirect (302)
ctx.redirect('/path', typ: .see_other) // redirect with GET (303)
ctx.request_error('bad request')   // 400
ctx.server_error('oops')           // 500
```

### Middleware

```v
pub struct App {
    veb.Middleware[Context]
}

pub fn check_auth(mut ctx Context) bool {
    token := ctx.get_cookie('token') or { '' }
    if token == '' {
        ctx.text('unauthorized')
        return false  // stop processing
    }
    return true  // continue
}

fn main() {
    mut app := &App{}
    app.use(handler: check_auth)        // global, before
    app.route_use('/api', handler: api_middleware)  // route-specific
    app.use(handler: log_response, after: true)     // after handler
    veb.run[App, Context](mut app, 8080)
}
```

### Static Files

```v
pub struct App {
    veb.StaticHandler
}

fn main() {
    mut app := &App{}
    app.handle_static('static', true)!  // mount at root
    app.mount_static_folder_at('assets', '/public')  // custom path
    app.serve_static('/logo.png', 'images/logo.png')!  // single file
    veb.run[App, Context](mut app, 8080)
}
```

### Controllers

```v
pub struct App {
    veb.Controller
}

pub struct Admin {}

pub fn (app &Admin) index(mut ctx Context) veb.Result {
    return ctx.text('Admin panel')
}

fn main() {
    mut app := &App{}
    mut admin_app := &Admin{}
    app.register_controller[Admin, Context]('/admin', mut admin_app)!
    veb.run[App, Context](mut app, 8080)
}
```

### Live Reload

```bash
v -d veb_livereload watch run .
```

## HTTP Client (net.http)

### Basic Requests

```v
import net.http

// GET
resp := http.get('https://example.com')!
println(resp.body)

// POST with string data
resp := http.post('https://api.example.com/data', '{"key":"value"}')!

// POST JSON
resp := http.post_json('https://api.example.com/json', '{"name":"Bob"}')!

// POST form data
resp := http.post_form('https://api.example.com/form', {'name': 'Bob'})!

// PUT, PATCH, DELETE
resp := http.put('https://api.example.com/item', 'data')!
resp := http.patch('https://api.example.com/item', 'data')!
resp := http.delete('https://api.example.com/item')!
```

### Advanced Requests with FetchConfig

```v
resp := http.fetch(http.FetchConfig{
    url:    'https://api.example.com/data'
    method: .post
    header: http.new_header(.content_type, 'application/json')
    data:   '{"key":"value"}'
    cookies: {'session': 'abc123'}
    user_agent: 'my-app/1.0'
    allow_redirect: true
    max_retries: 3
})!
println(resp.body)
println(resp.status_code)
```

### Response Handling

```v
resp := http.get('https://example.com')!

// Check status
if resp.status_code == 200 {
    println(resp.body)
}

// Parse status enum
status := resp.status()
if status.is_success() {
    println('OK')
}
if status.is_error() {
    println('Error: ${status.str()}')
}

// Get headers
content_type := resp.header.get(.content_type) or { 'unknown' }

// Get cookies
cookies := resp.cookies()
```

### Headers

```v
mut h := http.new_header(.content_type, 'application/json')
h.add(.accept, 'text/html')
h.set_custom('X-Custom', 'value')!

val := h.get(.content_type) or { '' }
exists := h.contains(.authorization)
h.delete(.cache_control)
```

### Download Files

```v
// Simple download
http.download_file('https://example.com/file.zip', 'output.zip')!

// With progress
http.download_file_with_progress(url, path, downloader: http.SilentStreamingDownloader{})!
```

### HTTP Server (low-level)

```v
import net.http
import net

struct MyHandler {}

fn (mut h MyHandler) handle(req http.Request) http.Response {
    return http.new_response(http.ResponseConfig{
        status: .ok
        body:   'Hello from server!'
    })
}

mut server := http.Server{
    addr:    ':8080'
    handler: MyHandler{}
}
server.listen_and_serve()
```
