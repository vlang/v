// vtest vflags: -autofree -gc none
// Copying a local with `a := b` or `a = b` must not let `a` take over `b`'s
// storage: reassigning `a` would free the value that `b` still uses.

struct Item {
mut:
	name string
	tags []string
}

fn parts(s string) []string {
	mut rest := s.trim(' ')
	mut out := []string{}
	for _ in 0 .. 2 {
		mut part := rest
		if rest.contains('-') {
			part = rest.all_before('-')
			rest = rest.all_after_first('-')
		}
		out << part
	}
	return out
}

fn copy_param(x string, n int) string {
	mut t := x
	if n > 0 {
		t = 'c'.repeat(n)
	}
	return x + t
}

fn assign_copy(n int) string {
	mut s := 'a'.repeat(n)
	mut t := 'b'.repeat(n)
	t = s
	t = 'c'.repeat(n)
	return s + t
}

fn copy_array(n int) []string {
	mut a := ['x'.repeat(n), 'y'.repeat(n)]
	mut b := a
	b = ['z'.repeat(n)]
	a << 'w'.repeat(n)
	return a
}

fn copy_struct(n int) string {
	mut it := Item{
		name: 'n'.repeat(n)
		tags: ['t'.repeat(n)]
	}
	mut c := it
	c = Item{
		name: 'm'.repeat(n)
	}
	return it.name + it.tags[0] + c.name
}

fn test_mut_copy_of_string_keeps_source() {
	assert parts(' 12-34 ') == ['12', '34']
	assert parts(' 56 ') == ['56', '56']
}

fn test_mut_copy_of_param_keeps_param() {
	x := 'z'.repeat(3)
	assert copy_param(x, 2) == 'zzzcc'
	assert x == 'zzz'
}

fn test_assigned_copy_keeps_source() {
	assert assign_copy(2) == 'aacc'
}

fn test_mut_copy_of_array_and_struct_keeps_source() {
	assert copy_array(1) == ['x', 'y', 'w']
	assert copy_struct(2) == 'nnttmm'
}
