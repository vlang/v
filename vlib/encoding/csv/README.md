## Reader example

```v
import encoding.csv

data := 'x,y\na,b,c\n'
mut parser := csv.new_reader(data)
// read each line
for {
	items := parser.read() or { break }
	println(items)
}
```

It prints:
```
['x', 'y']
['a', 'b', 'c']
```

Unquoted fields cannot contain double quotes. Enclose a field containing quotes in double
quotes and escape each embedded quote by doubling it; `read()` returns an error for a bare quote.

The writer preserves carriage returns and line feeds inside quoted fields by default. With
`use_crlf: true`, it drops carriage returns and writes each line feed as CRLF, matching the
record terminator. The reader normalizes CRLF to LF inside quoted fields.

The final record does not need a trailing line ending, including when the document contains
only one record. Empty documents and comment-only input contain no records.

A quoted field must have a closing quote, including in the final record. Truncated quoted
fields return `encoding.csv: unterminated quoted field`; clean end of input returns
`encoding.csv: end of file`.
