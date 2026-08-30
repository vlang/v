module imap

// A sequence set names the messages a command applies to (RFC 3501 section
// 9, `sequence-set`). It is a comma separated list of numbers and ranges, and
// `*` stands for the highest number in the mailbox.
//
// Ranges matter: a mailbox with fifty thousand messages is addressed as
// `1:50000`, where a list of every number would be a command line no server is
// obliged to accept.

// Zero is not a valid message number, which leaves it free to mean `*`.
const seq_star = u32(0)

// SeqRange is one number or one range of them. A single number has `start`
// equal to `stop`. A `stop` of zero means `*`, so `{5, 0}` reads `5:*` and
// `{0, 0}` reads `*`.
pub struct SeqRange {
pub:
	start u32
	stop  u32
}

// SeqSet is a set of message numbers, held as ranges that are kept sorted and
// merged so that the rendered form stays short.
pub struct SeqSet {
pub mut:
	ranges []SeqRange
}

// seq_set builds a set from a list of numbers.
pub fn seq_set(numbers []u32) SeqSet {
	mut s := SeqSet{}
	for n in numbers {
		s.add(n)
	}
	return s
}

// seq_range builds a set holding the single range `start:stop`.
pub fn seq_range(start u32, stop u32) SeqSet {
	mut s := SeqSet{}
	s.add_range(start, stop)
	return s
}

// seq_all is the set every message in the mailbox belongs to, `1:*`.
pub fn seq_all() SeqSet {
	return seq_range(1, seq_star)
}

// parse_seq_set reads a set back from its wire form, such as `2,4:7,9:*`.
pub fn parse_seq_set(s string) !SeqSet {
	mut out := SeqSet{}
	if s == '' {
		return out
	}
	for part in s.split(',') {
		colon := part.index(':') or {
			out.add(parse_seq_number(part)!)
			continue
		}
		out.add_range(parse_seq_number(part[..colon])!, parse_seq_number(part[colon + 1..])!)
	}
	return out
}

// add inserts one number.
pub fn (mut s SeqSet) add(n u32) {
	s.add_range(n, n)
}

// add_range inserts a range, in either order.
pub fn (mut s SeqSet) add_range(start u32, stop u32) {
	// `*` is the largest value there is, so it always ends a range.
	if start == seq_star {
		s.insert(SeqRange{stop, seq_star})
		return
	}
	if stop != seq_star && stop < start {
		s.insert(SeqRange{stop, start})
		return
	}
	s.insert(SeqRange{start, stop})
}

// len is the number of ranges the set is stored as, not the number of messages
// it names.
pub fn (s &SeqSet) len() int {
	return s.ranges.len
}

// is_empty reports whether the set names nothing, in which case no command
// should be sent at all.
pub fn (s &SeqSet) is_empty() bool {
	return s.ranges.len == 0
}

// contains reports whether `n` is named by the set. It answers for a concrete
// number; `*` is not resolvable without knowing the mailbox.
pub fn (s &SeqSet) contains(n u32) bool {
	for r in s.ranges {
		if r.start != seq_star && r.start <= n && (r.stop == seq_star || n <= r.stop) {
			return true
		}
	}
	return false
}

// numbers expands the set. It fails on a set holding `*`, whose end is only
// known to the server.
pub fn (s &SeqSet) numbers() ![]u32 {
	mut out := []u32{}
	for r in s.ranges {
		if r.start == seq_star || r.stop == seq_star {
			return error('imap: a set holding `*` cannot be expanded by the client')
		}
		for n := r.start; n <= r.stop; n++ {
			out << n
		}
	}
	return out
}

// str renders the set as a server reads it.
pub fn (s SeqSet) str() string {
	mut parts := []string{cap: s.ranges.len}
	for r in s.ranges {
		parts << r.str()
	}
	return parts.join(',')
}

// str renders one range, collapsing `n:n` back to `n`.
pub fn (r SeqRange) str() string {
	if r.start == r.stop {
		if r.start == seq_star {
			return '*'
		}
		return r.start.str()
	}
	if r.stop == seq_star {
		return '${r.start}:*'
	}
	return '${r.start}:${r.stop}'
}

// insert places a range and folds it into any neighbour it touches, which is
// what keeps `1,2,3,4` from ever being sent when `1:4` says the same thing.
fn (mut s SeqSet) insert(v SeqRange) {
	mut merged := v
	mut kept := []SeqRange{cap: s.ranges.len + 1}
	for r in s.ranges {
		union_range, ok := merge_ranges(merged, r)
		if ok {
			merged = union_range
			continue
		}
		kept << r
	}
	kept << merged
	kept.sort_with_compare(fn (a &SeqRange, b &SeqRange) int {
		// `*` sorts last, since it is the largest value a range can start at.
		if a.start == b.start {
			return 0
		}
		if a.start == seq_star {
			return 1
		}
		if b.start == seq_star {
			return -1
		}
		return if a.start < b.start { -1 } else { 1 }
	})
	s.ranges = kept
}

// merge_ranges unites two ranges when they overlap or sit next to each other,
// and reports whether they did.
fn merge_ranges(a SeqRange, b SeqRange) (SeqRange, bool) {
	if a == b {
		return a, true
	}
	// A range open at the top swallows anything that starts at or after it.
	if a.stop == seq_star && b.start != seq_star && b.start >= a.start {
		return a, true
	}
	if b.stop == seq_star && a.start != seq_star && a.start >= b.start {
		return b, true
	}
	if a.start == seq_star || b.start == seq_star || a.stop == seq_star || b.stop == seq_star {
		return a, false
	}
	mut lo := a
	mut hi := b
	if lo.start > hi.start {
		lo, hi = hi, lo
	}
	if lo.stop >= hi.stop {
		return lo, true
	}
	// Touching counts: 1:3 and 4:6 are the single range 1:6.
	if lo.stop >= hi.start || lo.stop + 1 == hi.start {
		return SeqRange{lo.start, hi.stop}, true
	}
	return a, false
}

fn parse_seq_number(s string) !u32 {
	if s == '*' {
		return seq_star
	}
	if s == '' {
		return error('imap: empty number in a sequence set')
	}
	for ch in s {
		if ch < `0` || ch > `9` {
			return error('imap: `${s}` is not a message number')
		}
	}
	n := s.u64()
	if n == 0 || n > 0xffffffff {
		return error('imap: `${s}` is out of the message number range')
	}
	return u32(n)
}
