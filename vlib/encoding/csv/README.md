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

A closing quote must be followed immediately by the configured delimiter or the end of the
record. `read()` returns an error when other characters, including spaces, follow a closing quote.
