import os
import strings
import benchmark

const max_iterations = os.getenv_opt('MAX_ITERATIONS') or { '100_000' }.int()

fn imin(x u16, y u16) u16 {
	return if x < y { x } else { y }
}

// From https://gist.github.com/zeozeozeo/f785910173f3115163bffd0c5240de07
@[direct_array_access]
pub fn zeozeozeo_levenshtein_distance(a string, b string) int {
	if a == '' {
		return b.len
	}
	if b == '' {
		return a.len
	}
	if a == b {
		return 0
	}

	mut row := []u16{len: a.len + 1}
	for i in 1 .. row.len {
		row[i] = i
	}

	for i := 1; i < b.len; i++ {
		mut prev := u16(i)
		for j := 1; j < a.len; j++ {
			mut current := row[j - 1] // match
			if b[i - 1] != a[j - 1] {
				// insertion, substitution, deletion
				current = imin(imin(row[j - 1] + 1, prev + 1), row[j] + 1)
			}
			row[j - 1] = prev
			prev = current
		}
		row[a.len] = prev
	}

	return row[a.len]
}

fn main() {
	a := 'abcdef'
	b := 'abdef'

	mut sum := i64(0)
	mut bench := benchmark.start()
	sum = 0
	for _ in 0 .. max_iterations {
		sum += i64(strings.levenshtein_distance(a, b))
	}
	bench.measure('strings.levenshtein_distance: ${sum}')

	sum = 0
	for _ in 0 .. max_iterations {
		sum += i64(zeozeozeo_levenshtein_distance(a, b))
	}
	bench.measure('zeozeozeo_levenshtein_distance: ${sum}')

	mut fsum := f64(0)
	for _ in 0 .. max_iterations {
		fsum += strings.dice_coefficient(a, b)
	}
	bench.measure('strings.dice_coefficient: ${fsum}')
}
