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
