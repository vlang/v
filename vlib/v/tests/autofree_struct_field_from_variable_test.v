// vtest vflags: -autofree -gc none
// A struct field set from a variable must store a copy: dropping the struct must
// not free a borrowed parameter, or a local that is still used afterwards.

struct Value {
	s string
}

fn (mut v Value) free() {}

struct Compiler {
	params map[string]Value
}

fn (c Compiler) count() int {
	return c.params.len
}

struct Bag {
	name  string
	items []string
	inner Value
}

struct Outer {
	c Compiler
	b Bag
}

fn compile(params map[string]Value) int {
	c := Compiler{
		params: params
	}
	return c.count()
}

fn bag(name string, items []string, inner Value) int {
	b := Bag{
		name:  name
		items: items
		inner: inner
	}
	return b.name.len + b.items.len + b.inner.s.len
}

fn local_still_used(n int) string {
	items := ['a'.repeat(n), 'b'.repeat(n)]
	b := Bag{
		items: items
	}
	assert b.items.len == 2
	return items.join(',')
}

fn nested(params map[string]Value, name string) int {
	o := Outer{
		c: Compiler{
			params: params
		}
		b: Bag{
			name: name
		}
	}
	return o.c.count() + o.b.name.len
}

fn test_borrowed_map_parameter() {
	params := {
		'a': Value{'x'.repeat(3)}
	}
	assert compile(params) == 1
	assert compile(params) == 1
	assert params['a'].s == 'xxx'
}

fn test_borrowed_parameters_of_each_kind() {
	name := 'n'.repeat(2)
	items := ['i'.repeat(2)]
	inner := Value{'v'.repeat(2)}
	assert bag(name, items, inner) == 5
	assert bag(name, items, inner) == 5
	assert name == 'nn'
	assert items == ['ii']
	assert inner.s == 'vv'
}

fn test_local_used_after_the_struct() {
	assert local_still_used(2) == 'aa,bb'
}

fn test_nested_struct_literal() {
	params := {
		'a': Value{'x'.repeat(3)}
	}
	name := 'n'.repeat(4)
	assert nested(params, name) == 5
	assert nested(params, name) == 5
	assert params['a'].s == 'xxx'
	assert name == 'nnnn'
}
