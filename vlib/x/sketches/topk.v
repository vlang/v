module sketches

pub struct TopKResult {
pub:
	key   string
	count u64
}

// SpaceSaving tracks the most frequent keys in bounded memory.
pub struct SpaceSaving {
pub:
	capacity int
mut:
	keys   []string
	counts []u64
}

pub fn new_space_saving(capacity int) !SpaceSaving {
	if capacity < 1 {
		return error('capacity must be at least 1')
	}
	return SpaceSaving{ capacity: capacity }
}

fn min_index(counts []u64) int {
	mut best := 0
	for i in 1 .. counts.len {
		if counts[i] < counts[best] {
			best = i
		}
	}
	return best
}

pub fn (mut s SpaceSaving) add(key string, count u64) {
	for i in 0 .. s.keys.len {
		if s.keys[i] == key {
			s.counts[i] += count
			return
		}
	}
	if s.keys.len < s.capacity {
		s.keys << key
		s.counts << count
		return
	}
	idx := min_index(s.counts)
	min_count := s.counts[idx]
	s.keys[idx] = key
	s.counts[idx] = min_count + count
}

pub fn (s &SpaceSaving) count(key string) u64 {
	for i in 0 .. s.keys.len {
		if s.keys[i] == key {
			return s.counts[i]
		}
	}
	return 0
}

pub fn (s &SpaceSaving) top(k int) []TopKResult {
	mut pairs := []TopKResult{len: s.keys.len}
	for i in 0 .. s.keys.len {
		pairs[i] = TopKResult{s.keys[i], s.counts[i]}
	}
	pairs.sort(a.count > b.count)
	n := if k > 0 && k < pairs.len { k } else { pairs.len }
	return pairs[..n].clone()
}
