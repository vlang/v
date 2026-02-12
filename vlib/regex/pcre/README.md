# regex.pcre Module Documentation

The `regex.pcre` module provides a **Virtual Machine (VM)** based regular expression engine with 
UTF-8 support.
Unlike recursive engines, this implementation uses an explicit heap stack, 
making it safe for complex patterns and long strings without risking stack overflows.

It supports compilation of patterns, searching, full matching, global replacement, named groups,
and iterative searching.

## Supported Syntax

| Feature | Syntax | Description |
| :--- | :--- | :--- |
| **Literals** | `abc` | Matches exact characters. |
| **Wildcard** | `.` | Matches any character (excluding `\n` unless `(?s)` flag is used). |
| **Alternation** | `|` | Matches the left OR right expression (e.g., `cat|dog`). |
| **Quantifiers** | `*` | Matches 0 or more times. |
| **Non-greedy quantifiers** | `*?`, `+?`, `??` | Avoid to consume as much as possible. |
| | `+` | Matches 1 or more times. |
| | `?` | Matches 0 or 1 time. |
| | `{m}` | Matches exactly `m` times. |
| | `{m,n}` | Matches between `m` and `n` times. |
| **Groups** | `(...)` | Capturing group. |
| | `(?:...)` | Non-capturing group. |
| | `(?P<name>...)` | Named capturing group. |
| **Anchors** | `^` | Matches start of string (or line start with `(?m)`). |
| | `$` | Matches end of string (or line end with `(?m)`). |
| | `\b` | Matches a word boundary (start/end of word). |
| | `\B` | Matches a non-word boundary. |
| **Classes** | `[abc]` | Matches any character in the set. |
| | `[^abc]` | Matches any character NOT in the set. |
| | `[a-z]` | Matches a range of characters. |
| | `\w`, `\W` | Word / Non-word character (`[a-zA-Z0-9_]`). |
| | `\d`, `\D` | Digit / Non-digit. |
| | `\s`, `\S` | Whitespace / Non-whitespace. |
| | `\a` | Lowercase character (`[a-z]`). |
| | `\A` | Uppercase character (`[A-Z]`). |
| **Escapes** | `\xHH` | Matches 1-byte hex value. |
| | `\XHHHH` | Matches 2-byte hex value. |
| **Flags** | `(?i)` | Case-insensitive matching. |
| | `(?m)` | Multiline mode (`^` and `$` match start/end of lines). |
| | `(?s)` | Dot-all mode (`.` matches `\n`). |

## Structs

### Regex
The compiled regular expression object containing the VM bytecode.
```v ignore
pub struct Regex {
pub:
    pattern      string
    total_groups int
    // Internal VM bytecode...
}
```

### Match
Represents the result of a successful search.
```v ignore
pub struct Match {
pub:
    text   string   // The full substring that matched
    start  int      // Start index in the source text
    end    int      // End index in the source text
    groups []string // List of captured groups
}
```

---

## Core Functions

### `compile`

Compiles a regular expression pattern string into a `Regex` object. Returns an error if the syntax
is invalid (e.g., unclosed groups).

```v ignore
fn compile(pattern string) !Regex
```

**Example:**
```v ignore
import regex.pcre

fn main() {
    // Compile a pattern to match a word followed by digits
    // The '?' after pcre.compile handles the result option
    r := pcre.compile(r'\w+\d+') or { panic(err) }
}
```

---

### `find`

Scans the text for the **first** occurrence of the pattern. Returns a `Match` object if found,
or `none` if not.

```v ignore
fn (r Regex) find(text string) ?Match
```

**Example:**
```v ignore
r := pcre.compile(r'(\d+)')!
text := "item 123, item 456"

if m := r.find(text) {
    println('Found: ${m.text}')   // Output: 123
    println('Index: ${m.start}')  // Output: 5
    println('Group 1: ${m.groups[0]}') // Output: 123
}
```

> **Note:** This function stops immediately after finding the leftmost match.

---

### `find_all`

Returns a list of **all non-overlapping** matches in the string. This is useful for extracting
multiple tokens.

```v ignore
fn (r Regex) find_all(text string) []Match
```

**Example:**
```v ignore
r := pcre.compile(r'\d+')!
text := "10, 20, 30"

matches := r.find_all(text)
for m in matches {
    println(m.text)
}
// Output:
// 10
// 20
// 30
```

> **Note:** If a pattern matches an empty string (e.g., `a*` on `"b"`), the engine automatically
advances the cursor by 1 to prevent infinite loops.

---

### `find_from`

Behaves like `find`, but starts scanning from a specific byte index. Useful for building lexers or
parsing text iteratively.

```v ignore
fn (r Regex) find_from(text string, start_index int) ?Match
```

**Example:**
```v 
import regex.pcre

r := pcre.compile(r'test')!
text := 'test test test'

// Skip the first 5 characters
if m := r.find_from(text, 5) {
	println('Found at: ${m.start}') // Output: Found at: 5
}
```

> **Note:** If `start_index` is out of bounds (< 0 or > len), it returns `none`.

---

### `fullmatch`

Checks if the **entire** string matches the pattern from start to end.

```v ignore
fn (r Regex) fullmatch(text string) ?Match
```

**Example:**
```v ignore
r := pcre.compile(r'\d{3}')!

println(r.fullmatch('123'))   // Match
println(r.fullmatch('1234'))  // none (too long)
println(r.fullmatch('a123'))  // none (starts with char)
```

---

### `replace`

Finds the **first** occurrence of the pattern and replaces it with the replacement string.

Supported backreferences:
*   `$1`, `$2`, etc. refer to captured groups.
*   `$0` is currently not supported.

```v ignore
fn (r Regex) replace(text string, repl string) string
```

**Example:**
```v
import regex.pcre

r := pcre.compile(r'(\w+), (\w+)')!
text := 'Doe, John'

// Swap groups
result := r.replace(text, '$2 $1')
println(result) // Output: "John Doe"
```

> **Note:** This function currently replaces only the *first* match found. 
To replace all occurrences,
you would need to loop using `replace` or reconstruct the string using `find_all` ranges.

---

### `group_by_name`

Retrieves the captured text for a specific named group defined with `(?P<name>...)`.

```v ignore
fn (r Regex) group_by_name(m Match, name string) string
```

**Example:**
```v ignore
import regex.pcre

r := pcre.compile(r'(?P<year>\d{4})-(?P<month>\d{2})')!
m := r.find('Date: 2025-01') or {pcre.Match{}}

year := r.group_by_name(m, 'year')
println(year) // Output: 2025
```

---

## Advanced Usage

### Non-greedy Matching
By default, quantifiers like `*` and `+` are **greedy**, meaning they match
as much text as possible. Adding a `?` makes them **non-greedy** (or lazy), 
matching the shortest possible string.

**Example:**
```v
import regex.pcre

text := '<div>content</div>'

// Greedy: Matches everything from the first '<' to the last '>'
r_greedy := pcre.compile(r'<.*>')!
println(r_greedy.find(text)?.text) // Output: <div>content</div>

// Non-greedy: Matches only until the first '>'
r_lazy := pcre.compile(r'<.*?>')!
println(r_lazy.find(text)?.text) // Output: <div>
```

### VM Stability (No Stack Overflow)
Because this engine uses a VM with a heap-allocated stack, it can handle patterns that typically
crash recursive engines due to stack overflow.

**Example:**
```v
import regex.pcre
// A pattern that causes catastrophic backtracking in some recursive engines
// or deep recursion depth.

r := pcre.compile(r'(a+)+b')!
text := 'a'.repeat(5000) // Very long string of 'a's

// This will safely return 'none' without crashing the program
r.find(text)
```

### Using Flags
Flags can be embedded to change matching behavior locally.

**Example:**
```v
import regex.pcre
// (?i) Case insensitive

r := pcre.compile(r'(?i)apple')!
println(r.find('APPLE')) // Matches

// (?m) Multiline: ^ matches start of line, $ matches end of line
r_multi := pcre.compile(r'(?m)^Log:')!
text := 'Error: 1\nLog: Something happened'
println(r_multi.find(text)) // Matches 'Log:' on the second line
```