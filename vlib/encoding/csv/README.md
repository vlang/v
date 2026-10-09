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

Quoted fields can span multiple lines. If the input ends before a quoted field closes,
`read()` returns `encoding.csv: unterminated quoted field`, distinct from normal end of file.
