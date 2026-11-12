// `for part in s.split('lit') {` scans lazily; everything else iterates the eager array.
// Both must yield exactly the parts of `split`.
type Csv = string

fn (c Csv) split(_ string) []string {
	return ['custom']
}

@[heap]
struct Counter {
mut:
	n int
}

fn eager(s string, delim string) []string {
	return s.split(delim)
}

fn lazy_comma(s string) []string {
	mut out := []string{}
	for part in s.split(',') {
		out << part
	}
	return out
}

fn lazy_colons(s string) []string {
	mut out := []string{}
	for part in s.split('::') {
		out << part
	}
	return out
}

fn lazy_aa(s string) []string {
	mut out := []string{}
	for part in s.split('aa') {
		out << part
	}
	return out
}

fn lazy_nul(s string) []string {
	mut out := []string{}
	for part in s.split('\x00') {
		out << part
	}
	return out
}

fn test_parts_match_split() {
	inputs := ['', ',', ',,', 'a', 'a,', ',a', 'a,b', 'a,,b', ',a,b,', 'ab,cd,ef', 'é,ü,∑',
		'::', 'a::b', '::a::', 'a:::b', 'a::::b', ':', 'aaa', 'aaaa', 'baab', 'a', 'x\x00y\x00',
		'\x00', 'no delimiter here']
	for s in inputs {
		assert lazy_comma(s) == eager(s, ','), s
		assert lazy_colons(s) == eager(s, '::'), s
		assert lazy_aa(s) == eager(s, 'aa'), s
		assert lazy_nul(s) == eager(s, '\x00'), s
	}
	long := 'abc,'.repeat(10000)
	assert lazy_comma(long) == eager(long, ',')
	assert lazy_colons('x'.repeat(3)) == ['xxx']
}

fn test_index_and_control_flow() {
	mut seen := []string{}
	mut idxs := []int{}
	for i, part in 'a,b,c,d,e'.split(',') {
		if part == 'b' {
			continue
		}
		if part == 'e' {
			break
		}
		idxs << i
		seen << part
	}
	assert idxs == [0, 2, 3]
	assert seen == ['a', 'c', 'd']
	mut skipped := 0
	for _, part in ',,x'.split(',') {
		if part == '' {
			skipped++
			continue
		}
	}
	assert skipped == 2
}

fn first_index(s string, want string) int {
	for i, part in s.split(';') {
		defer {
			_ = i
		}
		if part == want {
			return i
		}
	}
	return -1
}

fn test_return_and_defer() {
	assert first_index('a;b;c', 'c') == 2
	assert first_index('a;b;c', 'z') == -1
	assert first_index('', '') == 0
}

fn test_nested_and_labelled() {
	mut pairs := []string{}
	outer: for row in 'a=1;b=2;c=3'.split(';') {
		for j, cell in row.split('=') {
			if cell == '2' {
				continue outer
			}
			if row == 'c=3' && j == 1 {
				break outer
			}
			pairs << cell
		}
	}
	assert pairs == ['a', '1', 'b', 'c']
}

fn test_parts_are_owned_and_escape_safely() {
	mut kept := []string{}
	mut fns := []fn () string{}
	for part in 'x,y,z'.split(',') {
		kept << part
		fns << fn [part] () string {
			return part
		}
	}
	assert kept == ['x', 'y', 'z']
	assert fns.map(it()) == ['x', 'y', 'z']
	// Parts are NUL terminated like the eager ones.
	for part in 'ab,cd'.split(',') {
		assert unsafe { part.str[part.len] } == 0
	}
}

fn test_snapshot_and_single_evaluation() {
	mut text := 'a,b,c'
	mut out := []string{}
	for part in text.split(',') {
		text = 'z'
		out << part
	}
	assert out == ['a', 'b', 'c']
	assert text == 'z'
	mut c := &Counter{}
	next := fn [mut c] () string {
		c.n++
		return '1,2,3'
	}
	mut sum := 0
	for part in next().split(',') {
		sum += part.int()
	}
	assert sum == 6
	assert c.n == 1
}

fn test_snapshot_of_aliased_bytes() {
	mut data := 'a,b,c\x00'.bytes()
	// Borrow the NUL-terminated buffer so writes through data also change s.
	s := unsafe { tos(data.data, data.len - 1) }
	mut out := []string{}
	for part in s.split(',') {
		data[2] = `z`
		out << part
	}
	assert out == ['a', 'b', 'c']
	assert s == 'a,z,c'
}

fn test_indexed_snapshot_of_aliased_bytes_and_delimiters() {
	mut data := 'a::b::c\x00'.bytes()
	// Borrow the NUL-terminated buffer, including the delimiters being overwritten.
	s := unsafe { tos(data.data, data.len - 1) }
	mut out := []string{}
	mut indexes := []int{}
	for i, part in s.split('::') {
		data[3] = `z`
		data[4] = `x`
		if i == 0 {
			continue
		}
		out << part
		indexes << i
	}
	assert out == ['b', 'c']
	assert indexes == [1, 2]
	assert s == 'a::zx:c'
}

fn test_fallbacks() {
	// Empty delimiter splits by runes.
	mut runes := []string{}
	for part in 'aé∑'.split('') {
		runes << part
	}
	assert runes == ['a', 'é', '∑']
	// Custom `split`, non-literal delimiter and stored arrays keep the ordinary path.
	mut custom := []string{}
	for part in Csv('a,b').split(',') {
		custom << part
	}
	assert custom == ['custom']
	delim := ','
	mut dyn := []string{}
	for part in 'a,b'.split(delim) {
		dyn << part
	}
	assert dyn == ['a', 'b']
}
