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

## Writer line endings

By default, the writer ends records with `\n` and preserves `\r` and `\n` inside quoted fields.
With `csv.new_writer(use_crlf: true)`, records end with `\r\n`, and embedded `\n` is written as
`\r\n`. Existing `\r\n` pairs are preserved without adding another `\r`.
Bare `\r` is also preserved.
