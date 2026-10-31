module c

import strings

// headerless_libc_macro selects the libc surface of a merged portable compiler
// snapshot (see `merge_libc_variants`): 1 for V's own declarations, 0 for the
// system headers. Defining it on the C compiler's command line overrides the
// choice that the snapshot makes from the pointer width.
pub const headerless_libc_macro = '__V_HEADERLESS_LIBC'

const headerless_libc_prologue = '// The libc declarations that V writes itself have the layouts of 64-bit targets.
// On 32-bit targets, the C libraries disagree on them, so use the system headers.
#ifndef ${headerless_libc_macro}
#if (defined(__SIZEOF_POINTER__) && __SIZEOF_POINTER__ == 8) || (!defined(__SIZEOF_POINTER__) && (defined(_WIN64) || defined(__LP64__) || defined(_LP64)))
#define ${headerless_libc_macro} 1
#else
#define ${headerless_libc_macro} 0
#endif
#endif
'

// LibcVariantSegment is a run of units that both variants share (`common`), or a
// run where they differ: units `a_lo..a_hi` of the headerless variant against
// units `b_lo..b_hi` of the header-based one.
struct LibcVariantSegment {
mut:
	common bool
	a_lo   int
	a_hi   int
	b_lo   int
	b_hi   int
}

// merge_libc_variants merges two C files generated from the same program, one
// with V's own libc declarations (`headerless`) and one with the system headers
// (`headers`), into one file that preprocesses to the first when
// `__V_HEADERLESS_LIBC` is 1 and to the second when it is 0. The common parts
// appear once, and each differing run is wrapped in `#if`/`#else`/`#endif`.
// A run is widened until both of its sides are balanced conditional blocks, so
// the inserted directives nest correctly with the files' own.
pub fn merge_libc_variants(headerless string, headers string) string {
	a := c_preprocessor_units(headerless)
	b := c_preprocessor_units(headers)
	mut segments := []LibcVariantSegment{}
	align_libc_variant_units(a, b, 0, a.len, 0, b.len, mut segments)
	segments = coalesce_libc_variant_segments(segments)
	a_kinds := a.map(c_conditional_directive_kind(it))
	b_kinds := b.map(c_conditional_directive_kind(it))
	balance_libc_variant_segments(a_kinds, b_kinds, mut segments)
	mut sb := strings.new_builder(headerless.len + headers.len / 8)
	sb.write_string(headerless_libc_prologue)
	for seg in segments {
		if seg.common {
			write_c_units(mut sb, a, seg.a_lo, seg.a_hi)
			continue
		}
		if seg.a_lo == seg.a_hi {
			sb.writeln('#if !${headerless_libc_macro}')
			write_c_units(mut sb, b, seg.b_lo, seg.b_hi)
		} else {
			sb.writeln('#if ${headerless_libc_macro}')
			write_c_units(mut sb, a, seg.a_lo, seg.a_hi)
			if seg.b_lo < seg.b_hi {
				sb.writeln('#else')
				write_c_units(mut sb, b, seg.b_lo, seg.b_hi)
			}
		}
		sb.writeln('#endif')
	}
	return sb.str()
}

fn write_c_units(mut sb strings.Builder, units []string, lo int, hi int) {
	for i in lo .. hi {
		unit := units[i]
		sb.write_string(unit)
		if !unit.ends_with('\n') {
			sb.write_u8(`\n`)
		}
	}
}

// c_preprocessor_units splits C source into the units that a preprocessor guard
// can surround: physical lines, except that a line ending in a backslash and a
// block comment spanning lines stay with the lines they continue into. Each unit
// keeps its newline.
fn c_preprocessor_units(src string) []string {
	mut units := []string{cap: src.len / 48}
	mut start := 0
	mut i := 0
	mut in_comment := false
	mut quote := u8(0)
	for i < src.len {
		line_start := i
		for i < src.len && src[i] != `\n` {
			ch := src[i]
			if in_comment {
				if ch == `*` && i + 1 < src.len && src[i + 1] == `/` {
					in_comment = false
					i += 2
				} else {
					i++
				}
				continue
			}
			if quote != 0 {
				if ch == `\\` && i + 1 < src.len && src[i + 1] != `\n` {
					i += 2
					continue
				}
				if ch == quote {
					quote = 0
				}
				i++
				continue
			}
			if ch == `"` || ch == `'` {
				quote = ch
			} else if ch == `/` && i + 1 < src.len && src[i + 1] == `/` {
				for i < src.len && src[i] != `\n` {
					i++
				}
				break
			} else if ch == `/` && i + 1 < src.len && src[i + 1] == `*` {
				in_comment = true
				i += 2
				continue
			}
			i++
		}
		mut line_end := i
		if line_end > line_start && src[line_end - 1] == `\r` {
			line_end--
		}
		continued := line_end > line_start && src[line_end - 1] == `\\`
		if i < src.len {
			i++
		}
		if !continued {
			// A character or string literal cannot span lines.
			quote = 0
		}
		if !in_comment && !continued {
			units << src[start..i]
			start = i
		}
	}
	if start < src.len {
		units << src[start..]
	}
	return units
}

// c_conditional_directive_kind classifies a unit as a conditional directive:
// 1 opens a block (`#if`, `#ifdef`, `#ifndef`), -1 closes one (`#endif`), 2
// continues one (`#else`, `#elif`, ...), and 0 is anything else.
fn c_conditional_directive_kind(unit string) i8 {
	mut i := 0
	for i < unit.len && (unit[i] == ` ` || unit[i] == `\t`) {
		i++
	}
	if i >= unit.len || unit[i] != `#` {
		return 0
	}
	i++
	for i < unit.len && (unit[i] == ` ` || unit[i] == `\t`) {
		i++
	}
	start := i
	for i < unit.len && unit[i].is_letter() {
		i++
	}
	return match unit[start..i] {
		'if', 'ifdef', 'ifndef' { 1 }
		'endif' { -1 }
		'else', 'elif', 'elifdef', 'elifndef' { 2 }
		else { 0 }
	}
}

// align_libc_variant_units appends the segments that align `a[a_lo..a_hi]` with
// `b[b_lo..b_hi]`. It is a patience diff: after the common prefix and suffix, units
// that occur exactly once in both ranges anchor the alignment, the longest run of
// anchors in the same order is kept, and the gaps between them are aligned in turn.
// A gap without such anchors is one differing segment.
fn align_libc_variant_units(a []string, b []string, a_start int, a_end int, b_start int, b_end int, mut segments []LibcVariantSegment) {
	mut a_lo := a_start
	mut b_lo := b_start
	mut a_hi := a_end
	mut b_hi := b_end
	for a_lo < a_hi && b_lo < b_hi && a[a_lo] == b[b_lo] {
		a_lo++
		b_lo++
	}
	if a_lo > a_start {
		segments << LibcVariantSegment{
			common: true
			a_lo:   a_start
			a_hi:   a_lo
			b_lo:   b_start
			b_hi:   b_lo
		}
	}
	for a_hi > a_lo && b_hi > b_lo && a[a_hi - 1] == b[b_hi - 1] {
		a_hi--
		b_hi--
	}
	if a_lo < a_hi && b_lo < b_hi {
		anchors := libc_variant_anchors(a, b, a_lo, a_hi, b_lo, b_hi)
		if anchors.len == 0 {
			segments << LibcVariantSegment{
				a_lo: a_lo
				a_hi: a_hi
				b_lo: b_lo
				b_hi: b_hi
			}
		} else {
			mut pa := a_lo
			mut pb := b_lo
			for anchor in anchors {
				align_libc_variant_units(a, b, pa, anchor[0], pb, anchor[1], mut segments)
				segments << LibcVariantSegment{
					common: true
					a_lo:   anchor[0]
					a_hi:   anchor[0] + 1
					b_lo:   anchor[1]
					b_hi:   anchor[1] + 1
				}
				pa = anchor[0] + 1
				pb = anchor[1] + 1
			}
			align_libc_variant_units(a, b, pa, a_hi, pb, b_hi, mut segments)
		}
	} else if a_lo < a_hi || b_lo < b_hi {
		segments << LibcVariantSegment{
			a_lo: a_lo
			a_hi: a_hi
			b_lo: b_lo
			b_hi: b_hi
		}
	}
	if a_hi < a_end {
		segments << LibcVariantSegment{
			common: true
			a_lo:   a_hi
			a_hi:   a_end
			b_lo:   b_hi
			b_hi:   b_end
		}
	}
}

// libc_variant_anchors returns the pairs of positions of units that occur once in
// `a[a_lo..a_hi]` and once in `b[b_lo..b_hi]`, reduced to the longest list that is
// in the same order in both ranges.
fn libc_variant_anchors(a []string, b []string, a_lo int, a_hi int, b_lo int, b_hi int) [][2]int {
	mut b_pos := map[string]int{}
	for i in b_lo .. b_hi {
		if b[i] in b_pos {
			b_pos[b[i]] = -1
		} else {
			b_pos[b[i]] = i
		}
	}
	mut a_seen := map[string]int{}
	for i in a_lo .. a_hi {
		if a[i] in a_seen {
			a_seen[a[i]] = -1
		} else {
			a_seen[a[i]] = i
		}
	}
	mut pairs := [][2]int{}
	for i in a_lo .. a_hi {
		if a_seen[a[i]] != i {
			continue
		}
		j := b_pos[a[i]] or { continue }
		if j >= 0 {
			pairs << [i, j]!
		}
	}
	return longest_increasing_anchor_run(pairs)
}

// longest_increasing_anchor_run keeps the longest subsequence of `pairs` (ordered
// by their first position) whose second positions increase too.
fn longest_increasing_anchor_run(pairs [][2]int) [][2]int {
	if pairs.len == 0 {
		return []
	}
	// tails[k] is the index in `pairs` of the smallest end of a run of length k + 1.
	mut tails := []int{}
	mut prev := []int{len: pairs.len, init: -1}
	for i, pair in pairs {
		mut lo := 0
		mut hi := tails.len
		for lo < hi {
			mid := (lo + hi) / 2
			if pairs[tails[mid]][1] < pair[1] {
				lo = mid + 1
			} else {
				hi = mid
			}
		}
		if lo > 0 {
			prev[i] = tails[lo - 1]
		}
		if lo == tails.len {
			tails << i
		} else {
			tails[lo] = i
		}
	}
	mut run := [][2]int{len: tails.len}
	mut k := tails.last()
	for idx := tails.len - 1; idx >= 0; idx-- {
		run[idx] = pairs[k]
		k = prev[k]
	}
	return run
}

// coalesce_libc_variant_segments joins neighbouring segments of the same kind.
fn coalesce_libc_variant_segments(segments []LibcVariantSegment) []LibcVariantSegment {
	mut out := []LibcVariantSegment{cap: segments.len}
	for seg in segments {
		if seg.a_lo == seg.a_hi && seg.b_lo == seg.b_hi {
			continue
		}
		if out.len > 0 && out.last().common == seg.common {
			out[out.len - 1].a_hi = seg.a_hi
			out[out.len - 1].b_hi = seg.b_hi
			continue
		}
		out << seg
	}
	return out
}

// libc_variant_side_balance reports whether `kinds[lo..hi]` is a sequence of
// complete conditional blocks, and if not, whether the segment has to grow
// backwards (it closes or continues a block opened before it) rather than
// forwards (it opens a block that it does not close).
fn libc_variant_side_balance(kinds []i8, lo int, hi int) (bool, bool) {
	mut depth := 0
	for i in lo .. hi {
		match kinds[i] {
			1 {
				depth++
			}
			-1 {
				depth--
				if depth < 0 {
					return false, true
				}
			}
			2 {
				if depth == 0 {
					return false, true
				}
			}
			else {}
		}
	}
	return depth == 0, false
}

// balance_libc_variant_segments widens every differing segment, one unit at a
// time, into the neighbouring common units until both of its sides are balanced.
// Common units moved into a segment appear in both of its sides.
fn balance_libc_variant_segments(a_kinds []i8, b_kinds []i8, mut segments []LibcVariantSegment) {
	mut i := 0
	for i < segments.len {
		if segments[i].common {
			i++
			continue
		}
		a_ok, a_back := libc_variant_side_balance(a_kinds, segments[i].a_lo, segments[i].a_hi)
		b_ok, b_back := libc_variant_side_balance(b_kinds, segments[i].b_lo, segments[i].b_hi)
		if a_ok && b_ok {
			i++
			continue
		}
		if (a_back || b_back) && i > 0 {
			// Take the last unit of the common segment before this one.
			segments[i].a_lo--
			segments[i].b_lo--
			segments[i - 1].a_hi--
			segments[i - 1].b_hi--
			if segments[i - 1].a_lo == segments[i - 1].a_hi {
				segments.delete(i - 1)
				i--
				if i > 0 {
					// Now next to the previous differing segment: join the two.
					segments[i].a_lo = segments[i - 1].a_lo
					segments[i].b_lo = segments[i - 1].b_lo
					segments.delete(i - 1)
					i--
				}
			}
		} else if i + 1 < segments.len {
			// Take the first unit of the common segment after this one.
			segments[i].a_hi++
			segments[i].b_hi++
			segments[i + 1].a_lo++
			segments[i + 1].b_lo++
			if segments[i + 1].a_lo == segments[i + 1].a_hi {
				segments.delete(i + 1)
				if i + 1 < segments.len {
					segments[i].a_hi = segments[i + 1].a_hi
					segments[i].b_hi = segments[i + 1].b_hi
					segments.delete(i + 1)
				}
			}
		} else {
			// Both whole files are balanced, so a segment that reaches both ends is.
			i++
		}
	}
}

// libc_variant_source returns one of the two files merged by `merge_libc_variants`:
// the headerless one when `headerless` is true, the header-based one otherwise. It
// preprocesses only the guards of the merge. Every other directive stays as text,
// but its nesting is tracked, so it panics on guards that interleave with the
// files' own conditionals. Tests use it to inspect each side of a snapshot.
fn libc_variant_source(merged string, headerless bool) string {
	body := merged.all_after(headerless_libc_prologue)
	if body.len == merged.len {
		panic('libc_variant_source: the merged file does not start with its prologue')
	}
	mut out := []string{}
	// Each entry: -1 for a directive of the file, or 0/1 for whether the lines of a
	// guard of the merge are taken.
	mut stack := []int{}
	for line in body.split_into_lines() {
		taken := stack.all(it != 0)
		if line == '#if ${headerless_libc_macro}' {
			stack << if headerless { 1 } else { 0 }
			continue
		}
		if line == '#if !${headerless_libc_macro}' {
			stack << if headerless { 0 } else { 1 }
			continue
		}
		kind := c_conditional_directive_kind(line)
		if kind == 1 {
			stack << -1
		} else if kind == 2 && stack.len > 0 && stack.last() >= 0 {
			if line != '#else' {
				panic('libc_variant_source: `${line}` continues a guard of the merge')
			}
			stack[stack.len - 1] = 1 - stack.last()
			continue
		} else if kind == -1 {
			if stack.len == 0 {
				panic('libc_variant_source: unbalanced `#endif`')
			}
			top := stack.pop()
			if top >= 0 {
				continue
			}
		}
		if taken {
			out << line
		}
	}
	if stack.len != 0 {
		panic('libc_variant_source: unterminated conditional')
	}
	return out.join('\n')
}
