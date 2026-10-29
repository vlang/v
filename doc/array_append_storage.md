# Nested storage when appending a value

In the default compiler, appending a value copies the element in the same way as ordinary
assignment or an array literal. It does not implicitly call `clone()` or recursively copy
nested arrays or maps. Single-element append and bulk append therefore preserve the same sharing:

```v
struct Node {
mut:
	kids []int
}

fn main() {
	mut node := Node{ kids: [1] }
	copied := node
	literal := [node]
	mut single := []Node{}
	single << node
	mut bulk := []Node{}
	bulk << [node]

	node.kids[0] = 2
	assert copied.kids[0] == 2
	assert literal[0].kids[0] == 2
	assert single[0].kids[0] == 2
	assert bulk[0].kids[0] == 2
}
```

The struct and its array headers are copied, while the nested backing storage stays shared.
Changing an existing nested element is visible through the other copies. Replacing an array
field or changing its length does not update the other copies' headers. Growing an array can
allocate new backing storage. Fixed arrays store their elements inline and copy those elements
by value; nested dynamic arrays inside them still share backing storage.

Map fields also retain their ordinary shared storage when the containing struct is appended.

Request independent storage explicitly, for example `rows << row.clone()` for an array element.
For a map field, pass `value.m.clone()` in the appended struct initializer.
A user-defined struct `clone()` must copy the nested fields that need independent storage.
Appending the struct alone does not invoke that method.

This rule applies to the default memory management mode. Ownership builds and compatibility
`-autofree` retain their existing move, clone, and cleanup rules.
