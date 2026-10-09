## Writer example

```v
import encoding.csv

mut writer := csv.new_writer()
writer.write(['   ', 'b'])!
assert writer.str() == '"   ",b\n'
```

The writer quotes fields that contain the delimiter, double quotes, carriage returns, or newlines.
It also quotes fields that begin with Unicode whitespace, including spaces and tabs, preserving
them when read by CSV readers that trim unquoted leading whitespace.

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
