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

// The value `*` stands for while a set is being merged.
const seq_max = u32(0xffffffff)

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
	mut lo := to_bound(start)
	mut hi := to_bound(stop)
	if lo > hi {
		lo, hi = hi, lo
	}
	s.insert(lo, hi)
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
	if n == seq_star {
		return false
	}
	for r in s.ranges {
		lo, hi := bounds(r)
		if lo <= n && n <= hi {
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
		lo, hi := bounds(r)
		if hi == seq_max {
			return error('imap: a set holding `*` cannot be expanded by the client')
		}
		for n := lo; n <= hi; n++ {
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

// The ranges are kept sorted and disjoint, which lets insertion find its place
// by bisection instead of rescanning and re-sorting the whole set. Building a
// set of twenty thousand scattered numbers is the difference between a few
// milliseconds and ten seconds.

// bounds maps a range onto the plain interval it denotes, `*` becoming the
// largest number there is. Merging is then ordinary interval arithmetic rather
// than a nest of special cases.
fn bounds(r SeqRange) (u32, u32) {
	return to_bound(r.start), to_bound(r.stop)
}

fn to_bound(n u32) u32 {
	if n == seq_star {
		return seq_max
	}
	return n
}

fn from_bound(n u32) u32 {
	if n == seq_max {
		return seq_star
	}
	return n
}

// insert places the interval `lo:hi` and folds in every range it touches,
// which is what keeps `1,2,3,4` from ever being sent when `1:4` says the same
// thing.
fn (mut s SeqSet) insert(lo u32, hi u32) {
	// Bisect for the first range that reaches far enough to touch this one.
	mut first := 0
	mut last := s.ranges.len
	for first < last {
		mid := first + (last - first) / 2
		_, mid_hi := bounds(s.ranges[mid])
		if mid_hi < lo - 1 {
			first = mid + 1
			continue
		}
		last = mid
	}

	mut new_lo := lo
	mut new_hi := hi
	mut after := first
	for after < s.ranges.len {
		r_lo, r_hi := bounds(s.ranges[after])
		// Touching counts: 1:3 and 4:6 are the single range 1:6.
		if new_hi != seq_max && r_lo > new_hi + 1 {
			break
		}
		if r_lo < new_lo {
			new_lo = r_lo
		}
		if r_hi > new_hi {
			new_hi = r_hi
		}
		after++
	}

	merged := SeqRange{
		start: from_bound(new_lo)
		stop: from_bound(new_hi)
	}
	if after > first {
		s.ranges.delete_many(first, after - first)
	}
	s.ranges.insert(first, merged)
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
