# pcre Module Documentation

The `pcre` module is a high-performance **Virtual Machine (VM)** 
based regular expression engine for V. 

### Key Features
- **Non-recursive VM**: Safe execution that avoids stack overflows on complex patterns.
- **Zero-Allocation Search**: Uses a pre-allocated `Machine` workspace for search operations.
- **Fast ASCII Path**: Optimized path for characters < 128 to bypass heavy UTF-8 decoding.
- **Bitmap Lookups**: ASCII character classes use a 128-bit bitset for $O(1)$ matching.
- **Instruction Merging**: Consecutive character matches are merged
into string blocks for faster execution.

## Supported Syntax

| Feature | Syntax | Description |
| :--- | :--- | :--- |
| **Literals** | `abc` | Matches exact characters (UTF-8 supported). |
| **Wildcard** | `.` | Matches any character (excluding `\n` unless `(?s)` flag is used). |
| **Alternation** | `|` | Matches the left OR right expression (e.g., `cat|dog`). |
| **Quantifiers** | `*`, `+`, `?` | Matches 0+, 1+, or 0-1 times. |
| **Lazy** | `*?`, `+?`, `??` | Non-greedy versions of the above. |
| **Repetition** | `{m,n}` | Matches between `m` and `n` times. `{m,}` for m or more. |
| **Groups** | `(...)` | Capturing group. |
| | `(?:...)` | Non-capturing group. |
| | `(?P<name>...)` | Named capturing group. |
| **Anchors** | `^`, `$` | Start/End of string (or line with `(?m)`). |
| | `\b`, `\B` | Word boundary and Non-word boundary. |
| **Classes** | `[abc]`, `[^abc]` | Character set and Negated character set. |
| | `[a-z]` | Range of characters. |
| | `\w`, `\W` | Word / Non-word (`[a-zA-Z0-9_]`). |
| | `\d`, `\D` | Digit / Non-digit. |
| | `\s`, `\S` | Whitespace / Non-whitespace (` \t\n\r\v\f`). |
| | `\a`, `\A` | Lowercase / Uppercase ASCII character class. |
| **Flags** | `(?i)` | Case-insensitive matching. |
| | `(?m)` | Multiline mode (`^` and `$` match start/end of lines). |
| | `(?s)` | Dot-all mode (`.` matches newlines). |

---

## Structs

### Regex
The compiled regular expression object.
```v ignore
pub struct Regex {
pub:
    pattern      string         // The original pattern
    prog         []Inst         // Compiled VM bytecode
    total_groups int            // Number of capture groups
    group_map    map[string]int // Map for named groups
}
```

### Match
Represents the result of a successful search.
```v ignore
pub struct Match {
pub:
    text   string   // The full substring that matched
    start  int      // Byte index where match starts
    end    int      // Byte index where match ends
    groups []string // Text captured by each group
}
```

---

## Core Functions

### `compile`
Compiles a pattern into a `Regex` object.
```v ignore
fn compile(pattern string) !Regex
```

### `find`
Finds the first match in the text. Returns `none` if no match is found.
```v ignore
fn (r Regex) find(text string) ?Match
```

### `find_all`
Returns all non-overlapping matches in a string.
```v ignore
fn (r Regex) find_all(text string) []Match
```

### `replace`
Replaces the first match in `text` with `repl`.
Supports backreferences like `$1`, `$2`.
```v ignore
fn (r Regex) replace(text string, repl string) string
```

### `change_stack_depth`
Updates the maximum backtracking depth for the VM.
Default is 1024.
Use this if your pattern is extremely complex and returns `none` prematurely.
```v ignore
fn (mut r Regex) change_stack_depth(depth int)
```

---

## Named Groups Example

```v
import regex.pcre

fn main() {
	r := pcre.compile(r'(?P<year>\d{4})-(?P<month>\d{2})')!
	m := r.find('Date: 2026-02') or { return }

	year := r.group_by_name(m, 'year')
	month := r.group_by_name(m, 'month')
	println('Year: ${year}, Month: ${month}') // Year: 2026, Month: 02
}
```

---

## PCRE Compatibility Layer

To facilitate easier migration from other engines, a compatibility layer is provided:

| Function | Equivalent To |
| :--- | :--- |
| `new_regex(pattern, flags)` | `compile(pattern)` |
| `r.match_str(text, start, flags)` | `r.find_from(text, start)` |
| `m.get(idx)` | Retrieves match text (`0`) or capture group (`1+`). |
| `m.get_all()` | Returns `[full_match, group1, group2, ...]` |

**Example:**
```v
import regex.pcre

r := pcre.new_regex(r'(\w+) (\w+)', 0)!
if m := r.match_str('hello world', 0, 0) {
	println(m.get(0)?) // "hello world"
	println(m.get(1)?) // "hello"
	println(m.get(2)?) // "world"
}
```

## Performance Note
The engine automatically detects literal prefixes (e.g., in `abc.*`) and uses
a fast-skip optimization to bypass the VM until the prefix is found in the 
input string. 
This makes it extremely fast for searching specific patterns in large files.