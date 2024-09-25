fn one() string {
	name := 'Peter'
	age := 25
	numbers := [1, 2, 3]
	downloads := {
		'vlang/ui':  '3201'
		'vlang/vtl': '123'
	}
	ignored := true
	return $tmpl('tmpl/base.txt')
}

fn outside_return() string {
	name := 'Peter'
	age := 25
	numbers := [1, 2, 3]
	downloads := {
		'vlang/ui':  '3201'
		'vlang/vtl': '123'
	}
	ignored := true
	result := $tmpl('tmpl/base.txt')
	return result
}

fn test_tmpl() {
	expected := "name: Peter
age: 25
numbers: [1, 2, 3]

1
2
3

0 - 0
2 - 1
4 - 2
6 - 3
8 - 4
10 - 5
12 - 6
14 - 7
16 - 8
18 - 9

vlang/ui, downloaded 3201 times.
vlang/vtl, downloaded 123 times.

this is not ignored

so, it's basically true"

	assert one().trim_space() == expected
	assert outside_return().trim_space() == expected
}

fn test_tmpl_in_anon_fn() {
	anon := fn (name string, age int, numbers []int, downloads map[string]string, ignored bool) string {
		return $tmpl('tmpl/base.txt')
	}

	assert anon('Peter', 25, [1, 2, 3], {
		'vlang/ui':  '3201'
		'vlang/vtl': '123'
	}, true).trim_space() == "name: Peter
age: 25
numbers: [1, 2, 3]

1
2
3

0 - 0
2 - 1
4 - 2
6 - 3
8 - 4
10 - 5
12 - 6
14 - 7
16 - 8
18 - 9

vlang/ui, downloaded 3201 times.
vlang/vtl, downloaded 123 times.

this is not ignored

so, it's basically true"
}

fn test_tmpl_interpolation() {
	my_var := 'foo'
	s := $tmpl('tmpl/interpolation.txt')
	assert s == 'result: foo\n'
}

fn my_fn(s string) string {
	return s
}

// Add more examples of potentially buggy patterns in vlib/v/tests/tmpl/index.html
fn test_tmpl_comptime() {
	index := $tmpl('tmpl/index.html').trim_space()
	// dump(index)
	assert index.contains('<br>Line ending with percent %\n')
	assert index.contains('<br>Line ending with at $\n')
	assert index.contains('<br>Line ending with ampersand &\n')
	assert index.contains('<br>Line ending with hash #\n')
	assert index.contains('<br>Line ending with slash /\n')
	assert index.contains('<br>Line ending with dollar $\n')
	assert index.contains('<br>Line ending with caret ^\n')
}

// Add a tests for @include

// File contents for building repsonse
const base = '<p>This is the base file</p>'
const child = '<p>This is the child file</p>'
const grandchild = '<p>This is the grandchild file</p>'
const parent = '<p>This is the parent file</p>'

// Call the parent file which contains all child templates
fn test_tmpl_include_parent() {
	expected := [base, base, child, base, base, child, grandchild, parent].join('\n')
	parent_tmpl := $tmpl('tmpl/parent.html')
	assert parent_tmpl.contains(expected)
}

// Test the child template which calls parent template
fn test_tmpl_include_child() {
	expected := [base, child].join('\n')
	child_tmpl := $tmpl('tmpl/nested/child.html')
	assert child_tmpl.contains(expected)
}

// Test the grandchild templates calling both parent and grandparent templates
fn test_tmpl_include_grandchild() {
	expected := [base, base, child, grandchild].join('\n')
	child_tmpl := $tmpl('tmpl/nested/nested_deeper/grandchild.html')
	assert child_tmpl.contains(expected)
}
