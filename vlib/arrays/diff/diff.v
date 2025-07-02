module diff

import strings

// DiffChange contains one or more deletions or inserts at one position in two arrays.
pub struct DiffChange {
pub mut:
	a   int // position in input a []T
	b   int // position in input b []T
	del int // delete Del elements from input a
	ins int // insert Ins elements from input b
}

@[flag]
enum DiffContextFlag {
	delete
	insert
}

pub struct DiffContext[T] {
mut:
	a     []T
	b     []T
	flags []DiffContextFlag
	max   int
	// forward and reverse d-path endpoint x components
	forward []int
	reverse []int
pub mut:
	changes []DiffChange
}

// diff returns the difference of two arrays.
pub fn diff[T](a []T, b []T) &DiffContext[T] {
	mut c := &DiffContext[T]{
		a: a
		b: b
	}
	c.flags = if a.len > b.len {
		[]DiffContextFlag{len: a.len}
	} else {
		[]DiffContextFlag{len: b.len}
	}
	c.max = a.len + b.len + 1
	c.forward = []int{len: 2 * c.max}
	c.reverse = []int{len: 2 * c.max}
	c.compare(0, 0, a.len, b.len)
	c.changes = c.result(a.len, b.len)
	return c
}

// A directly conversion from https://github.com/covrom/diff
// Fast diff library for Myers algorithm.
// The algorithm is described in "An O(ND) Difference Algorithm and its Variations", Eugene Myers, Algorithmica Vol. 1 No. 2, 1986, pp. 251-266
@[direct_array_access]
fn (mut c DiffContext[T]) compare(mut_aoffset int, mut_boffset int, mut_alimit int, mut_blimit int) {
	mut aoffset := mut_aoffset
	mut boffset := mut_boffset
	mut alimit := mut_alimit
	mut blimit := mut_blimit
	// eat common prefix
	for aoffset < alimit && boffset < blimit && c.a[aoffset] == c.b[boffset] {
		aoffset++
		boffset++
	}
	// eat common suffix
	for alimit > aoffset && blimit > boffset && c.a[alimit - 1] == c.b[blimit - 1] {
		alimit--
		blimit--
	}
	// both equal or b inserts
	if aoffset == alimit {
		for boffset < blimit {
			c.flags[boffset].set(.insert)
			boffset++
		}
		return
	}
	// a deletes
	if boffset == blimit {
		for aoffset < alimit {
			c.flags[aoffset].set(.delete)
			aoffset++
		}
		return
	}
	x, y := c.find_middle_snake(aoffset, boffset, alimit, blimit)
	c.compare(aoffset, boffset, x, y)
	c.compare(x, y, alimit, blimit)
}

@[direct_array_access]
fn (mut c DiffContext[T]) find_middle_snake(aoffset int, boffset int, alimit int, blimit int) (int, int) {
	// midpoints
	fmid := aoffset - boffset
	rmid := alimit - blimit
	// correct offset in d-path slices
	foff := c.max - fmid
	roff := c.max - rmid
	isodd := (rmid - fmid) & 1 != 0
	maxd := (alimit - aoffset + blimit - boffset + 2) / 2
	c.forward[c.max + 1] = aoffset
	c.reverse[c.max - 1] = alimit
	mut x, mut y := 0, 0
	for d := 0; d <= maxd; d++ {
		// forward search
		for k := fmid - d; k <= fmid + d; k += 2 {
			if k == fmid - d || (k != fmid + d && c.forward[foff + k + 1] > c.forward[foff + k - 1]) {
				x = c.forward[foff + k + 1] // down
			} else {
				x = c.forward[foff + k - 1] + 1 // right
			}
			y = x - k
			for x < alimit && y < blimit && c.a[x] == c.b[y] {
				x++
				y++
			}
			c.forward[foff + k] = x
			if isodd && k > rmid - d && k < rmid + d {
				if c.reverse[roff + k] <= c.forward[foff + k] {
					return x, x - k
				}
			}
		}
		// reverse search x,y correspond to u,v
		for k := rmid - d; k <= rmid + d; k += 2 {
			if k == rmid + d || (k != rmid - d && c.reverse[roff + k - 1] < c.reverse[roff + k + 1]) {
				x = c.reverse[roff + k - 1] // up
			} else {
				x = c.reverse[roff + k + 1] - 1 // left
			}
			y = x - k
			for x > aoffset && y > boffset && c.a[x - 1] == c.b[y - 1] {
				x--
				y--
			}
			c.reverse[roff + k] = x
			if !isodd && k >= fmid - d && k <= fmid + d {
				if c.reverse[roff + k] <= c.forward[foff + k] {
					// lookup opposite end
					x = c.forward[foff + k]
					return x, x - k
				}
			}
		}
	}
	panic('diff.find_middle_snake: should never be reached')
}

@[direct_array_access]
fn (c DiffContext[T]) result(n int, m int) []DiffChange {
	mut x, mut y := 0, 0
	mut res := []DiffChange{}
	for x < n || y < m {
		if x < n && y < m && !c.flags[x].has(.delete) && !c.flags[y].has(.insert) {
			x++
			y++
		} else {
			mut a := x
			mut b := y
			for x < n && (y >= m || c.flags[x].has(.delete)) {
				x++
			}
			for y < m && (x >= n || c.flags[y].has(.insert)) {
				y++
			}
			if a < x || b < y {
				res << DiffChange{a, b, x - a, y - b}
			}
		}
	}
	return res
}

// merge_changes merges neighboring changes smaller than the specified context_lines.
// The changes must be ordered by ascending positions.
@[direct_array_access]
fn (mut c DiffContext[T]) merge_changes(context_lines int) {
	if c.changes.len == 0 {
		return
	}

	mut merged := []DiffChange{}
	mut current := c.changes[0]

	for i in 1 .. c.changes.len {
		next := c.changes[i]
		if next.a <= current.a + current.del + context_lines {
			current = DiffChange{
				a:   current.a
				b:   current.b
				del: next.a + next.del - current.a
				ins: next.b + next.ins - current.b
			}
		} else {
			merged << current
			current = next
		}
	}
	merged << current
	c.changes = merged
}

@[params]
pub struct DiffGenStrParam {
pub mut:
	colorful     bool
	unified      int = 3 // how many context lines before/after diff block
	block_header bool // output `@@ -3,4 +3,5 @@` or not
}

// generate_patch generate a diff string of two arrays.
@[direct_array_access]
pub fn (mut c DiffContext[T]) generate_patch(param DiffGenStrParam) string {
	mut sb := strings.new_builder(100)
	defer { unsafe { sb.free() } }

	mut unified := if param.unified < 0 { 0 } else { param.unified }

	c.merge_changes(unified)
	if c.changes.len == 0 {
		return ''
	}

	mut prev_a_end := 0
	mut prev_b_end := 0

	for change in c.changes {
		ctx_start_a := int_max(prev_a_end, change.a - unified)
		ctx_end_a := change.a + change.del + unified
		ctx_start_b := int_max(prev_b_end, change.b - unified)
		ctx_end_b := change.b + change.ins + unified

		if param.block_header {
			if param.colorful {
				sb.write_string('\033[36m')
			}
			sb.writeln('@@ -${ctx_start_a + 1},${ctx_end_a - ctx_start_a} +${ctx_start_b + 1},${ctx_end_b - ctx_start_b} @@')
			if param.colorful {
				sb.write_string('\033[0m')
			}
		}

		c.write_context(mut sb, ctx_start_b, change.b, param)
		c.write_change(mut sb, change, param)
		c.write_context(mut sb, change.b + change.ins, ctx_end_b, param)

		prev_a_end = ctx_end_a
		prev_b_end = ctx_end_b
	}

	return sb.str()
}

@[direct_array_access]
fn (c DiffContext[T]) write_context(mut sb strings.Builder,
	start int, end int,
	param DiffGenStrParam) {
	for i in start .. end {
		if i >= c.b.len {
			break
		}

		line := c.b[i].str()

		if param.colorful {
			sb.writeln('\033[37m${line}\033[0m')
		} else {
			sb.writeln(line)
		}
	}
}

@[direct_array_access]
fn (c DiffContext[T]) write_change(mut sb strings.Builder,
	change DiffChange,
	param DiffGenStrParam) {
	for i in change.a .. change.a + change.del {
		line := c.a[i].str()
		if param.colorful {
			sb.writeln('\033[31m-${line}\033[0m')
		} else {
			sb.writeln('-${line}')
		}
	}

	for i in change.b .. change.b + change.ins {
		line := c.b[i].str()
		if param.colorful {
			sb.writeln('\033[32m+${line}\033[0m')
		} else {
			sb.writeln('+${line}')
		}
	}
}
